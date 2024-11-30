unit Umemoria;
{$codepage UTF8}

interface
Uses UlistaProcesos, crt, UlistaSimple, SysUtils, Classes, StrUtils;    

Type 
    Tlista=^Tnodo;
    Tnodo=record 
        sig,ant:Tlista;
        id,peso,dir_comienzo:integer;
        estado:boolean;
        proceso:ulistaProcesos.Tproceso;
        end;

    Tmemoria = record
        TamanoMemoria:          Integer;  // Tamaño de la memoria en KB
        Estrategia:             string;   // Estrategia de asignación de particiones
        TiempoSeleccion:        Integer;  // Tiempo de selección de partición (en ms)
        TiempoCarga:            Integer;  // Tiempo de carga promedio (en ms)
        TiempoLiberacion:       Integer;  // Tiempo de liberación de partición (en ms)
        lista:                  Tlista;
        ultimaAsignacion:       Tlista;
        FragExterna:            integer;
        Num_Particiones:        integer;
    end;


procedure init(var memoria:Tmemoria);

function pushFF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple): boolean;
function pushNF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple): boolean;
function pushBF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple): boolean;
function pushWF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple): boolean;

function empty(memoria:Tmemoria):boolean;
procedure updateMemoria(var memoria: Tmemoria; tiempo: byte; var Cantidad_De_Procesos: integer; var Eventos, Resultados : UlistaSimple.TlistaSimple );


implementation

procedure init(var memoria:Tmemoria);
var 
    aux:Tlista;
begin
    new(aux);
    aux^.sig:=nil;
    aux^.ant:=nil;
    aux^.id:=0;
    aux^.dir_comienzo:=0;
    aux^.peso:= memoria.TamanoMemoria;
    aux^.estado:=true;
    memoria.FragExterna:=0;
    memoria.lista:=aux;
    memoria.Num_Particiones:=0;
    memoria.ultimaAsignacion := memoria.lista; // Inicialmente, la última asignación es la primera partición

end;

function pushFF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple): boolean;
var 
    aux, nuevaparticion: Tlista; nota:String;

begin
    aux := memoria.lista; // Comenzamos desde el inicio de la lista

    // Buscamos una particion libre con suficiente espacio
    while (aux <> nil) and ((aux^.peso < proceso.peso) or (aux^.estado <> true)) do
        aux := aux^.sig;
    
    // Si encontramos una partición adecuada
    if aux <> nil then
    begin
        Nota := ('El Proceso "' + proceso.id + '" de peso ' + intToStr(proceso.peso) + ' se inserta en la particion "' + intToStr(aux^.id) + '"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);



        // Si hay espacio sobrante, creamos una nueva particion
        if aux^.peso > proceso.peso then
        begin
            new(nuevaparticion); // Creamos una nueva particion
            nuevaparticion^.sig := aux^.sig;
            nuevaparticion^.ant := aux;
            if aux^.sig <> nil then
                aux^.sig^.ant := nuevaparticion; // Actualizamos el anterior del siguiente nodo
            aux^.sig := nuevaparticion;
            nuevaparticion^.peso := aux^.peso - proceso.peso; // Espacio restante
            nuevaparticion^.estado := true; // Libre
            nuevaparticion^.id := memoria.Num_Particiones + 1; // Nuevo ID
            inc(memoria.Num_Particiones); // Incrementamos el contador de particiones
            nuevaparticion^.dir_comienzo:=aux^.dir_comienzo+aux^.peso+1;

            Nota:= ('Se crea una nueva particion libre"'+ intToStr(nuevaparticion^.id) + '" con un tamaño de '+intToStr(nuevaparticion^.peso));
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);
        end;

        // Asignamos el proceso a la particion encontrada
        aux^.proceso := proceso;
        aux^.peso := proceso.peso; // Ajustamos el tamaño de la particion
        aux^.estado := false; // La particion ahora está ocupada
        pushFF:=true;
    end
    else
    begin
        Nota:= ('No se encontro espacio suficiente para el proceso "'+ proceso.id+ '"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);
        pushFF:=false;
    end;
end;

function pushNF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple ): boolean;
var
    aux, nuevaparticion,inicioBusqueda: Tlista;
    pesoOriginal: integer; // Para guardar el peso original de la partición
     // Para detectar si completamos un ciclo
    nota:String;
begin
    
    // Si no hay última asignación, comenzamos desde el inicio.
    if (memoria.ultimaAsignacion = nil) then
        memoria.ultimaAsignacion := memoria.lista;

    aux := memoria.ultimaAsignacion; // Comenzamos desde la última asignación realizada.
    inicioBusqueda := aux;           // Guardamos el punto de inicio para detectar un ciclo.

    // Recorremos la lista buscando una partición adecuada
    while (aux <> nil) and ((aux^.peso < proceso.peso) or (aux^.estado = false)) do
    begin
        aux := aux^.sig; // Avanzamos al siguiente nodo

        // Si llegamos al final de la lista, volvemos al inicio
        if aux = nil then
            aux := memoria.lista;

        // Si completamos un ciclo sin encontrar espacio, salimos del bucle
        if aux = inicioBusqueda then
            break;
    end;

    // Si encontramos una partición adecuada
    if (aux <> nil) and (aux^.peso >= proceso.peso) and (aux^.estado = true) then
    begin
        
        Nota:= ('El Proceso "'+ proceso.id+ '" de peso' + intToStr(proceso.peso)+
                ' se inserta en la partición "'+ intToStr(aux^.id)+ '"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);


        // Guardamos el peso original antes de modificarlo
        pesoOriginal := aux^.peso;

        // Si hay espacio sobrante, creamos una nueva partición
        if pesoOriginal > proceso.peso then
        begin
            new(nuevaparticion); // Creamos una nueva partición
            nuevaparticion^.sig := aux^.sig;
            nuevaparticion^.ant := aux;
            if aux^.sig <> nil then
                aux^.sig^.ant := nuevaparticion; // Actualizamos el anterior del siguiente nodo
            aux^.sig := nuevaparticion;
            nuevaparticion^.peso := pesoOriginal - proceso.peso; // Espacio restante
            nuevaparticion^.estado := true; // Libre
            nuevaparticion^.id := memoria.Num_Particiones + 1; // Nuevo ID
            inc(memoria.Num_Particiones); // Incrementamos el contador de particiones
            nuevaparticion^.dir_comienzo := aux^.dir_comienzo + proceso.peso;

            
            Nota:= ('Se crea una nueva partición libre "'+ intToStr(nuevaparticion^.id)+ 
                    '" con un tamaño de '+ intToStr(nuevaparticion^.peso));
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);

        end;

        // Asignamos el proceso a la partición encontrada
        aux^.proceso := proceso;
        aux^.peso := proceso.peso; // Ajustamos el tamaño de la partición
        aux^.estado := false; // La partición ahora está ocupada

        // Actualizamos la última asignación realizada
        memoria.ultimaAsignacion := aux;

        pushNF := true;
    end
    else
    begin
        Nota:= ('No se encontró espacio suficiente para el proceso "'+proceso.id+'"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);
        pushNF := false;
    end;
end;

function pushBF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple ): boolean;
var 
    LaTuya, aux, nuevaparticion: Tlista;
    nota:String;
begin
    
    aux := memoria.lista; // Comenzamos desde el inicio de la lista

    new(LaTuya);
    LaTuya^.id:=-1;         //La logica de que sea -1 es de que no hay particiones disponibles.
    LaTuya^.peso:=10000;    //El peso 
    // Buscamos una particion libre con suficiente espacio
    while (aux <> nil) do
    begin
        //Estado: Si esta libre u ocupado. {True = Libre// false = Ocupado}
        if (aux^.estado = true) and (aux^.peso >= proceso.peso) then
        begin
            if (aux^.peso < LaTuya^.peso) then
            begin
            LaTuya:=aux;
            end;
        end;
        aux := aux^.sig;
    end;
    aux:=LaTuya;
    // Si encontramos una partición adecuada
    if aux^.id <> -1 then
    begin
        Nota:=( 'El Proceso "'+ proceso.id+ '" de peso '+ intToStr(proceso.peso)+ ' se inserta en la particion "'+ intToStr(aux^.id)+ '"' );
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

        // Si hay espacio sobrante, creamos una nueva particion
        if aux^.peso > proceso.peso then
        begin
            new(nuevaparticion); // Creamos una nueva particion
            nuevaparticion^.sig := aux^.sig;
            nuevaparticion^.ant := aux;
            if aux^.sig <> nil then
                aux^.sig^.ant := nuevaparticion; // Actualizamos el anterior del siguiente nodo
            aux^.sig := nuevaparticion;
            nuevaparticion^.peso := aux^.peso - proceso.peso; // Espacio restante
            nuevaparticion^.estado := true; // Libre
            nuevaparticion^.id := memoria.Num_Particiones + 1; // Nuevo ID
            inc(memoria.Num_Particiones); // Incrementamos el contador de particiones
            nuevaparticion^.dir_comienzo:=aux^.dir_comienzo+aux^.peso+1;

            Nota:= ('Se crea una nueva particion libre"'+ intToStr(nuevaparticion^.id)+
                    '" con un tamaño de '+ intToStr(nuevaparticion^.peso));
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);

        end;

        // Asignamos el proceso a la particion encontrada
        aux^.proceso := proceso;
        aux^.peso := proceso.peso; // Ajustamos el tamaño de la particion
        aux^.estado := false; // La particion ahora está ocupada
        pushBF:=true;
    end
    else
    begin

        Nota:= ('No se encontró espacio suficiente para el proceso "'+proceso.id+'"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);
        
        pushBF:=false;
    end;
end;

function pushWF(var memoria: Tmemoria; proceso:UlistaProcesos.Tproceso; var Eventos: UlistaSimple.TlistaSimple ): boolean;
var 
    boquita, aux, nuevaparticion: Tlista;
    nota:String;
begin
    

    aux := memoria.lista; // Comenzamos desde el inicio de la lista

    new(boquita);
    boquita^.id:=-1;
    boquita^.peso:=0;
    // Buscamos una particion libre con suficiente espacio
    while (aux <> nil) do
    begin
        if (aux^.estado = true) and (aux^.peso >= proceso.peso)and(aux^.peso > boquita^.peso) then
        begin
            write('(pase)');
            boquita:=aux;
        end;
        aux := aux^.sig;
    end;
    aux:=boquita;
    // Si encontramos una partición adecuada
    if aux^.id <> -1 then
    begin

        Nota:= ('El Proceso "'+ proceso.id+ '" de peso' + intToStr(proceso.peso)+
                ' se inserta en la partición "'+ intToStr(aux^.id)+ '"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

        // Si hay espacio sobrante, creamos una nueva particion
        if aux^.peso > proceso.peso then
        begin
            new(nuevaparticion); // Creamos una nueva particion
            nuevaparticion^.sig := aux^.sig;
            nuevaparticion^.ant := aux;
            if aux^.sig <> nil then
                aux^.sig^.ant := nuevaparticion; // Actualizamos el anterior del siguiente nodo
            aux^.sig := nuevaparticion;
            nuevaparticion^.peso := aux^.peso - proceso.peso; // Espacio restante
            nuevaparticion^.estado := true; // Libre
            nuevaparticion^.id := memoria.Num_Particiones + 1; // Nuevo ID
            inc(memoria.Num_Particiones); // Incrementamos el contador de particiones
            nuevaparticion^.dir_comienzo:=aux^.dir_comienzo+aux^.peso+1;

            Nota:= ('Se crea una nueva particion libre"'+ intToStr(nuevaparticion^.id)+
                    '" con un tamaño de '+ intToStr(nuevaparticion^.peso));
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);
            
        end;

        // Asignamos el proceso a la particion encontrada
        aux^.proceso := proceso;
        aux^.peso := proceso.peso; // Ajustamos el tamaño de la particion
        aux^.estado := false; // La particion ahora está ocupada
        pushWF:=true;
    end
    else
    begin

        Nota:= ('No se encontro espacio suficiente para el proceso "'+ proceso.id+ '"');
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

        pushWF:=false;
    end;
end;


function empty(memoria:Tmemoria):boolean;
begin
    empty:= (memoria.lista <> nil) and 
            (memoria.lista^.estado = true) and 
            (memoria.lista^.sig = nil) and 
            (memoria.lista^.ant = nil);
end;

procedure liberarMemoria(var lista: Tlista; var Num_Particiones: integer;var memoria:Tmemoria;Cantidad_De_Procesos:integer; var Eventos: TlistaSimple );
var
    aux: Tlista;
    updateId: boolean;
    nota:String;
begin
    

    updateId := false;
    lista^.estado := true; // Marcamos la partición como libre

    Nota:= ('Se libera la partición: '+ intToStr(lista^.id)+'. Que albergaba al proceso: '+lista^.proceso.id);
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

    // Verificar y unir con la siguiente partición si está libre
    if (lista^.sig <> nil) and (lista^.sig^.estado = true) then
    begin
        if Cantidad_De_Procesos>0 then
        memoria.FragExterna+=lista^.sig^.peso;

        Nota:= ('La partición '+ intToStr(lista^.id)+ ' se une con la partición '+ intToStr(lista^.sig^.id));
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

        updateId := true;
        aux := lista^.sig;
        lista^.peso += aux^.peso; // Incrementar el peso con la siguiente partición
        lista^.sig := aux^.sig;   // Saltar el nodo unido
        if aux^.sig <> nil then
            aux^.sig^.ant := lista; // Actualizar el puntero anterior del siguiente nodo
        dispose(aux); // Liberar el nodo unido
    end;

    // Verificar y unir con la anterior partición si está libre
    if (lista^.ant <> nil) and (lista^.ant^.estado = true) then
    begin

        Nota:= ('La partición '+ intToStr(lista^.id) + ' se une con la partición '+ intToStr(lista^.ant^.id));
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

        updateId := true;
        aux := lista^.ant;
        aux^.peso += lista^.peso; // Incrementar el peso con la actual partición
        aux^.sig := lista^.sig;   // Saltar el nodo actual
        if lista^.sig <> nil then
            lista^.sig^.ant := aux; // Actualizar el puntero anterior del siguiente nodo
        dispose(lista); // Liberar el nodo actual
        lista := aux;   // Actualizar la referencia a la partición unida
    end;

    // Actualizar el ID de la partición si hubo uniones
    if updateId then
    begin
        lista^.id := Num_Particiones + 1; // Nuevo ID único para la partición unida

        Nota:= ('Dadas las uniones entre particiones, se crea una nueva partición '+ intToStr(lista^.id)+
                ' de tamaño: '+ intToStr(lista^.peso));
        writeln(Nota);
        ListaSimplePush(Eventos,Nota);

        inc(Num_Particiones); // Incrementamos el contador global de particiones
    end;
end;


procedure updateMemoria(var memoria: Tmemoria; tiempo: byte; var Cantidad_De_Procesos: integer; var Eventos, Resultados : UlistaSimple.TlistaSimple );
var aux:Tlista;
    nota:String;

    valorFe:String;
    valorTiempo: String;
    ValorParticion: String;
    ValorTiempoLiberacion: String;
begin
    aux:= memoria.lista;
    while (aux <> nil) do
    begin
        aux^.proceso.duracion-= 1;
        if aux^.proceso.duracion-memoria.TiempoLiberacion = 0 then 
        begin

            valorTiempo:= IntToStr(tiempo);
            ValorParticion:= IntToSTR(aux^.id);
            ValorTiempoLiberacion:= IntToStr(memoria.TiempoLiberacion);

            Nota:= ('El proceso '+aux^.proceso.id+' finalizo en el tiempo: '+ valorTiempo +'. la particion '+ ValorParticion +' se liberara en '+ ValorTiempoLiberacion+'ms.');
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);

            nota:=('Tiempo de retorno del proceso '+aux^.proceso.id+': '+intToStr(tiempo));
            writeln(Nota);
            ListaSimplePush(Resultados,Nota);
        end;
        if (Cantidad_De_Procesos>0)and(aux^.estado=true) then
        begin
            memoria.FragExterna+= aux^.peso;

            ValorFe:= intToStr(memoria.FragExterna);
            Nota:= 'fe: '+ ValorFe;
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);

        end;
        if aux^.proceso.duracion=0 then 
            liberarMemoria(aux, memoria.Num_Particiones, memoria, Cantidad_De_Procesos, Eventos);
        aux:=aux^.sig;
    end;
end;

    
end.