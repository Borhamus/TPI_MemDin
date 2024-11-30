Unit UlistaSimple;

interface

Type 
    TlistaSimple = ^Tproceso;

    Tproceso = record 
        info: String;
        sig: TlistaSimple;
    end;

procedure init(var lista: TlistaSimple);
procedure ListaSimplePush(var lista: TlistaSimple; info: String);
procedure ListaSimplePop(var lista: TlistaSimple; var info: String);

implementation

// Inicializa la lista a nil.
procedure init(var lista: TlistaSimple);
begin
    lista := nil;
end;

// Inserta un nuevo nodo con 'info' al final de la lista.
procedure ListaSimplePush(var lista: TlistaSimple; info: String);
var
    aux, aux2: TlistaSimple;
begin
    new(aux);               // Crea el nuevo nodo
    aux^.info := info;      // Asigna la información al nodo
    aux^.sig := nil;        // El nuevo nodo apunta a nil

    if lista = nil then     // Si la lista está vacía, inserta al principio
    begin
        lista := aux;
    end
    else
    begin
        aux2 := lista;
        // Avanza hasta el último nodo
        while aux2^.sig <> nil do 
            aux2 := aux2^.sig; 
        aux2^.sig := aux;   // Inserta el nuevo nodo al final
    end;
end;

// Elimina el primer nodo de la lista y devuelve su información en 'info'.
procedure ListaSimplePop(var lista: TlistaSimple; var info: String);
var 
    aux: TlistaSimple;
begin
    if lista <> nil then    // Si la lista no está vacía
    begin
        aux := lista;       // Guarda el primer nodo
        info := lista^.info;  // Obtiene la información del nodo
        lista := lista^.sig;  // Mueve la cabeza de la lista al siguiente nodo
        dispose(aux);        // Libera la memoria del nodo extraído
    end;
end;

end.
