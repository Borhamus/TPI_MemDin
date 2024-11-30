unit UlistaProcesos;
interface
    type 
        Tproceso = record
            id: string;
            arribo, retorno: byte;
            Duracion: byte;
            peso: integer; // tamaño
        end;

    Tlista = ^Tnodo;
    Tnodo = record 
        info: Tproceso;
        sig: Tlista;
    end;

    procedure init(var lista: Tlista);
    function empty(lista: Tlista): boolean;
    procedure pushOrdenado(var lista: Tlista; proceso: Tproceso);
    procedure pushNormal(var lista: Tlista; proceso: Tproceso);
    procedure pop(var lista: Tlista; var proceso: Tproceso);
    procedure print(lista: Tlista);

implementation

// Inicializa la lista a nil.
procedure init(var lista: Tlista);
begin
    lista := nil;
end;

// Verifica si la lista está vacía.
function empty(lista: Tlista): boolean;
begin
    empty := lista = nil;
end;

// Inserta un nuevo proceso ordenado por su tiempo de arribo.
procedure pushOrdenado(var lista: Tlista; proceso: Tproceso);
var 
    aux, actual, anterior: Tlista;
begin
    new(aux);
    aux^.info := proceso;
    aux^.sig := nil;

    if (lista = nil) or (lista^.info.Arribo > proceso.Arribo) then  
    begin
        aux^.sig := lista;
        lista := aux;
    end
    else                
    begin
        anterior := nil;
        actual := lista;
        while (actual <> nil) and (actual^.info.Arribo <= proceso.Arribo) do
        begin
            anterior := actual;
            actual := actual^.sig;
        end;
        // Insertar el nuevo nodo en la posición correcta
        anterior^.sig := aux;
        aux^.sig := actual;
    end;
end;

// Inserta un nuevo proceso al final de la lista.
procedure pushNormal(var lista: Tlista; proceso: Tproceso);
var 
    aux, anterior: Tlista; 
begin
    new(aux);
    aux^.info := proceso;
    aux^.sig := nil;

    if (lista = nil) then
    begin
        lista := aux; // Inserta si la lista está vacía
    end
    else
    begin
        anterior := lista;
        while anterior^.sig <> nil do 
            anterior := anterior^.sig;
        anterior^.sig := aux; // Inserta al final
    end;
end;

// Elimina el primer proceso de la lista.
procedure pop(var lista: Tlista; var proceso: Tproceso);
var 
    aux: Tlista;
begin
    if lista <> nil then 
    begin
        proceso := lista^.info;
        aux := lista;
        lista := lista^.sig;
        dispose(aux);  // Libera el nodo extraído
    end;
end;

// Imprime todos los procesos de la lista.
procedure print(lista: Tlista);
begin
    writeln('Lista de procesos:');
    while lista <> nil do
    begin
        writeln('id de proceso: ', lista^.info.id, 
                ', Arribo: ', lista^.info.Arribo,
                ', Duracion: ', lista^.info.Duracion, 
                ', peso: ', lista^.info.peso);
        lista := lista^.sig;
    end;
end;

end.
