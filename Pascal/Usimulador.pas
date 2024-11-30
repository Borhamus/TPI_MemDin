unit Usimulador;
{$codepage UTF8}

interface

uses
    SysUtils, Crt, Classes, StrUtils, UlistaProcesos, Umemoria, UlistaSimple;

procedure SimularTanda();

implementation

//========== x. Leer números con validación ==========
function LeerNumeroEnRango(Min, Max: Integer): Integer;
var
    valor: Integer;
begin
    repeat
        ReadLn(valor);
        if (valor < Min) or (valor > Max) then
            WriteLn('Por favor ingrese un número entre ', Min, ' y ', Max, '.');
    until (valor >= Min) and (valor <= Max);
    LeerNumeroEnRango := valor;
end;


//========== x. Mostrar lista de tandas ==========
procedure MostrarTandas(var opcion: Integer; var NombreArchivo: string);
var
    Info: TSearchRec;
    Archivos: array of string;
    cantidadTandas: Integer;
begin
    SetLength(Archivos, 0);  // Inicializa el arreglo vacío
    cantidadTandas := 0;

    // Buscar los archivos en la carpeta 'file/tandas'
    if FindFirst('file/tandas/*.txt', faAnyFile, Info) = 0 then
    begin
        WriteLn('Lista de tandas disponibles:');
        
        repeat
            Inc(cantidadTandas); // Incrementar el contador
            SetLength(Archivos, cantidadTandas);  // Redimensionar el arreglo solo una vez al final del bucle
            Archivos[cantidadTandas - 1] := Info.Name; // Agregar el archivo encontrado

            WriteLn(cantidadTandas, ') ', Info.Name);  // Imprimir el nombre del archivo
            
        until FindNext(Info) <> 0;

        FindClose(Info);  // Cerrar la búsqueda

        // Si hay tandas disponibles, pedir al usuario que elija una
        if cantidadTandas > 0 then
        begin
            Write('Seleccione una tanda (1-', cantidadTandas, '): ');
            opcion := LeerNumeroEnRango(1, cantidadTandas);  // Leer la opción seleccionada
            NombreArchivo := Archivos[opcion - 1];  // Asignar el nombre del archivo seleccionado a NombreArchivo
        end
        else
            WriteLn('No hay tandas disponibles.');
    end
    else
    begin
        WriteLn('No se encontraron tandas disponibles en la carpeta.');
    end;
end;

//========== 2. Cargar tanda seleccionada ==========
procedure CargarTandaEn2Listas(NombreDeLaTandaSeleccionada: String; var ListaOriginal, ListaCopia : UlistaProcesos.Tlista; var cantReq:Integer);
var
    archivo: TextFile;
    linea: string;
    nuevoProceso: UlistaProcesos.Tproceso;
begin
    // Usar el parámetro 'NombreDeLaTandaSeleccionada' para abrir el archivo correcto
    Assign(archivo, 'file/tandas/' + NombreDeLaTandaSeleccionada);  
    Reset(archivo);

    while not Eof(archivo) do
    begin
        ReadLn(archivo, linea);
        with nuevoProceso do
        begin
            id   := ExtractWord(1, linea, [',']);
            Arribo   := StrToInt(ExtractWord(2, linea, [',']));
            Duracion := StrToInt(ExtractWord(3, linea, [',']));
            peso     := StrToInt(ExtractWord(4, linea, [',']));
        end;

        UlistaProcesos.pushOrdenado(ListaOriginal, nuevoProceso);
        UlistaProcesos.pushOrdenado(ListaCopia,    nuevoProceso);
        inc(cantReq);
    end;

    Close(archivo);
end;

//========== 3. Configurar simulador ==========
procedure ConfigurarSimulador(var Memoria: Tmemoria);
var
    opcion: Integer;
begin
    WriteLn('Configurar simulador:');
    Writeln('Tamaño de la memoria (KB): ');
    Writeln('(Recuerda que minimo, la memoria debe ser de 10kb y maximo 1024kb) ');
    Memoria.TamanoMemoria:= LeerNumeroEnRango(10, 1024);
    Writeln('');

    WriteLn('Seleccione la estrategia de asignación:');
    WriteLn('1) First-Fit');
    WriteLn('2) Best-Fit');
    WriteLn('3) Next-Fit');
    WriteLn('4) Worst-Fit');
    opcion := LeerNumeroEnRango(1, 4);

    case opcion of
        1: Memoria.Estrategia := 'First-Fit';
        2: Memoria.Estrategia := 'Best-Fit';
        3: Memoria.Estrategia := 'Next-Fit';
        4: Memoria.Estrategia := 'Worst-Fit';
    end;
    WriteLn('');

    Writeln('Tiempo de selección de partición (ms): ');
    Writeln('(Recuerda que minimo 0 y maximo 100ms) ');
    Memoria.TiempoSeleccion:= LeerNumeroEnRango(0, 100);
    Writeln('');

    Writeln('Tiempo de carga promedio (ms): ');
    Writeln('(Recuerda que minimo 0 y maximo 100ms) ');
    Memoria.TiempoCarga:= LeerNumeroEnRango(0, 100);
    Writeln('');

    Writeln('Tiempo de liberación de partición (ms): ');
    Writeln('(Recuerda que minimo 0 y maximo 100ms) ');
    Memoria.TiempoLiberacion:= LeerNumeroEnRango(0, 100);
    Writeln('');

    Umemoria.init(memoria);
end;

//========== x. Obtener el valor correcto de la nueva simulacion que se va a crear ==========
function ObtenerNumeroArchivoSimulacion(): Integer;
var
    Info: TSearchRec;
    MaxNumero, NumeroActual: LongInt;  // Cambiar a LongInt
begin
    MaxNumero := 0;
    NumeroActual := 0;  // Inicializar la variable para evitar la advertencia

    // Buscar archivos en la carpeta de simulaciones
    if FindFirst('file/simulaciones/*.txt', faAnyFile, Info) = 0 then
    begin
        repeat
            // Extraer el número al inicio del archivo usando un patrón
            if TryStrToInt(ExtractWord(1, Info.Name, ['_']), NumeroActual) then
            begin
                if NumeroActual > MaxNumero then
                    MaxNumero := NumeroActual;
            end;

        until FindNext(Info) <> 0;
        FindClose(Info);
    end;

    // El siguiente número será el mayor + 1
    ObtenerNumeroArchivoSimulacion := MaxNumero + 1;
end;

//========== 4. Procedimiento Que genera la simulacion ==========
procedure CrearSimulacion(var ListaOriginal : UlistaProcesos.Tlista; var Memoria: Tmemoria; var Eventos , Resultados: UlistaSimple.TlistaSimple; cantReq:integer);
var
    tiempo:integer;
    desbloqueado:boolean; 
    proceso:UlistaProcesos.Tproceso;
    Nota:String;                    //Para guardar eventos y resultados.
    Cantidad_De_Procesos: Integer;

    Finalaiser:string;

begin
    clrscr; 
    Cantidad_De_Procesos:= cantReq;
    
    tiempo:= 0;
    desbloqueado:= true;

    while (Cantidad_De_Procesos > 0) or not (Umemoria.empty(memoria)) do
    begin
        UlistaProcesos.print(ListaOriginal);
        writeln('Tiempo: ' , tiempo);
        Nota:= ('Tiempo: ' + IntToStr(tiempo));
        UlistaSimple.ListaSimplePush(Eventos, Nota);

        // Si hay procesos por leer y no hay uno bloqueado actualmente
        if (Desbloqueado = true)  then
            UlistaProcesos.pop(ListaOriginal, proceso); // Sacamos un nuevo proceso de la lista //A VER

        // Verifica si el proceso es mayor a la memoria total
        if proceso.peso > memoria.TamanoMemoria  then
        begin
            Nota:= 'Esta simulación fue detenida. El proceso es mayor a la memoria total.';
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);
            Nota:= 'Peso de la memoria: ' + IntToStr(memoria.TamanoMemoria) + '.';
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);
            Nota:= 'Peso del proceso: ' + IntToStr(proceso.peso) + '.';
            writeln(Nota);
            ListaSimplePush(Eventos,Nota);
            break;
        end;

        // Si el tiempo actual coincide con el tiempo de arribo del proceso
        if (tiempo >= proceso.arribo) and (Cantidad_De_Procesos > 0) then
        begin
            if memoria.Estrategia = 'First-Fit' then
            Desbloqueado := Umemoria.pushFF(memoria, proceso, Eventos)
            else 
            if memoria.Estrategia = 'Best-Fit' then
            Desbloqueado := Umemoria.pushBF(memoria, proceso, Eventos)
            else 
            if memoria.Estrategia = 'Next-Fit' then
            Desbloqueado := Umemoria.pushNF(memoria, proceso, Eventos)
            else 
            if memoria.Estrategia = 'Worst-Fit' then
            Desbloqueado := Umemoria.pushWF(memoria, proceso, Eventos);
        end;

        if desbloqueado then Cantidad_De_Procesos-= 1;
        // Actualiza el estado de la memoria
        Umemoria.updateMemoria(memoria, tiempo, Cantidad_De_Procesos, Eventos, Resultados);
        // Incrementa el tiempo
        tiempo += 1;
    end;

    Nota := 'fragmentacion externa FE: ' + IntToStr(memoria.FragExterna) + '.';
    writeln(Nota);
    ListaSimplePush(Resultados,Nota);

    Nota := 'Finalizacion de la tanda: ' + IntToStr(tiempo - memoria.TiempoLiberacion - 1) + '.';
    writeln(Nota);
    ListaSimplePush(Resultados,Nota);

    Nota := 'Fin de la simulacion';
    writeln(Nota);
    ListaSimplePush(Resultados,Nota);

    
    readln(Finalaiser);
    write(Finalaiser);

end;


//========== 5. Procedimiento Que guarda la simulación ========== 
procedure GuardarSimulacion(var ListaCopia: UlistaProcesos.Tlista; Memoria: Tmemoria; var Eventos, Resultados: UlistaSimple.TlistaSimple; cantReq: Integer);
var
    archivoNombre: string;
    archivoNumero: Integer;
    archivo: TextFile;
    Viajero: TlistaSimple;
    eventInfo: string;
    numStr: string;
    archivoNumeroExtraido: Integer;  // Cambiado a Integer para asegurar suma correcta
    ResultadoTotal: Integer;         // Cambiado a Integer para asegurar suma correcta
    verga: Real;
    Finalaiser: String;
    posDosPuntos: Integer;  // Para almacenar la posición del ':'
    errorCode: Integer;
begin
    UlistaSimple.Init(Viajero);

    archivoNumero := ObtenerNumeroArchivoSimulacion();
    archivoNombre := Format('file/simulaciones/%d_simulacion(%d)_estrategia(%s).txt', [archivoNumero, cantReq, Memoria.Estrategia]);
    Assign(archivo, archivoNombre);
    Rewrite(archivo);

    WriteLn('===========================================================');
    WriteLn(archivo, 'Simulación:');
    WriteLn(archivo, '- Número de simulación: ', archivoNumero);
    WriteLn(archivo, '- Número de Requerimientos: ', cantReq);
    WriteLn('===========================================================');
    WriteLn(archivo, 'Configuración:');
    WriteLn(archivo, '- Tamaño de la memoria: ', Memoria.TamanoMemoria, ' KB');
    WriteLn(archivo, '- Estrategia de asignación: ', Memoria.Estrategia);
    WriteLn(archivo, '- Tiempo de selección de partición: ', Memoria.TiempoSeleccion, ' ms');
    WriteLn(archivo, '- Tiempo de carga promedio: ', Memoria.TiempoCarga, ' ms');
    WriteLn(archivo, '- Tiempo de liberación de partición: ', Memoria.TiempoLiberacion, ' ms');
    WriteLn('===========================================================');
    WriteLn(archivo, 'Eventos:');
    
    Viajero := Eventos;
    while Viajero <> nil do
    begin
        WriteLn(archivo, Viajero^.info); // Escribe cada evento en el archivo
        Viajero := Viajero^.sig;
    end;

    WriteLn(archivo, '===========================================================');
    Viajero := Resultados;
    WriteLn(archivo, 'Resultados:');
    
    // Inicializamos la variable ResultadoTotal
    ResultadoTotal := 0;

    Viajero := Resultados;
    while Viajero <> nil do
    begin
        WriteLn(archivo, Viajero^.info); // Escribe cada resultado en el archivo
        
        // Obtener el texto después de ":"
        eventInfo := Viajero^.info;
        
        // Buscar la posición del ':' en el string
        posDosPuntos := Pos(':', eventInfo);
        
        // Si se encuentra un ':', extraemos el número que viene después
        if posDosPuntos > 0 then
        begin
            // Extraemos el número (todo lo que sigue después del ':')
            numStr := Trim(Copy(eventInfo, posDosPuntos + 1, Length(eventInfo) - posDosPuntos));

            // Convertimos el número extraído a Integer (para evitar errores con decimales)
            Val(numStr, archivoNumeroExtraido, errorCode);
            
            if errorCode = 0 then  // Si la conversión fue exitosa
            begin
                ResultadoTotal := ResultadoTotal + archivoNumeroExtraido;  // Acumula el total   
            end;
        end;

        Viajero := Viajero^.sig;
    end;

    // Escribe el ResultadoTotal al final del archivo
    WriteLn(archivo, 'Resultado total de los retornos de los procesos: ', ResultadoTotal);
    
    // Calcular el tiempo de retorno normalizado
    if cantReq > 0 then
    begin
        verga := ResultadoTotal / cantReq;
        // Limitar la cantidad de decimales y convertir a string con el formato adecuado
        WriteLn(archivo, 'Tiempo de Retorno Normalizado: ', FormatFloat('0.0', verga));  // Formato adecuado con 1 decimal
    end
    else
    begin
        WriteLn(archivo, 'Tiempo de Retorno Normalizado: 0 (Error: cantReq es 0)');
    end;

    // Escribe la finalización de la simulación
    WriteLn(archivo, '===========================================================');
    WriteLn(archivo, 'Simulación Finalizada.');
    Writeln('Simulación Finalizada!');

    Close(archivo);
    WriteLn('Archivo creado: ', archivoNombre);
    Writeln('Para ver el resultado correctamente,');
    Writeln('vaya al menú principal y seleccione el archivo simulación que acaba de crear');

    readln(Finalaiser);
end;










//==============================================================
//========== 0. Procedimiento principal: SimularTanda ==========
//==============================================================
procedure SimularTanda();
var
    opcion: Integer;
    NombreDeLaTandaSeleccionada:String; cantReq: Integer;
    
    ListaOriginal,ListaCopia: UlistaProcesos.Tlista; 
    Memoria: Umemoria.TMemoria;
    Eventos, Resultados: UlistaSimple.TlistaSimple;
begin
    //Paso 0: Inicializo listas..
    cantReq:=0;
    UlistaProcesos.init(ListaOriginal);
    UlistaProcesos.init(ListaCopia);
    
    UlistaSimple.init(Eventos);
    UlistaSimple.init(Resultados);
    
    // Paso 1: Mostrar y seleccionar tanda
    MostrarTandas(opcion,NombreDeLaTandaSeleccionada);
    if opcion = 0 then Exit;

    // Paso 2: Cargar tanda seleccionada
    CargarTandaEn2Listas(NombreDeLaTandaSeleccionada,ListaOriginal, ListaCopia, cantReq);

    // Paso 3: Configurar simulador
    ConfigurarSimulador(Memoria);

    // Paso 4: Crear simulación
    CrearSimulacion(ListaOriginal, Memoria, Eventos, Resultados, cantReq);
    

    // Paso 5: Guardar resultados en un archivo
    GuardarSimulacion(ListaCopia, Memoria, Eventos, Resultados, cantReq);

    WriteLn('Presione Enter para regresar al menú.');
    ReadLn;
end;

end.
