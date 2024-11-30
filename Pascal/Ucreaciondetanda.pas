unit Ucreaciondetanda;
{$codepage UTF8}
//#=========== Interface =============================
// Declaración de tipos, procedimientos y funciones que estarán disponibles para otros módulos

interface

uses
    SysUtils,  // Para manejo de cadenas, archivos y funciones del sistema
    Classes;   // Para trabajar con listas y clases

// Tipo de dato para la lista enlazada
type
    Tlista = ^Tnodo;  // Puntero al nodo de la lista

    Tnodo = record
        info: string;  // Información del proceso (cadena de texto)
        sig: Tlista;   // Puntero al siguiente nodo
    end;

// Procedimiento para crear una nueva tanda de procesos
procedure CrearNuevaTanda();

implementation

//#=========== Lista Enlazada ========================

// Inicializa la lista como vacía
procedure InicializarLista(var lista: Tlista);
begin
    lista := nil;  // Apunta la lista a nil
end;

// Inserta un nodo con información al final de la lista
procedure InsertarAlFinal(var lista: Tlista; valor: string);
var
    nuevoNodo, aux: Tlista;
begin
    // Crear un nuevo nodo
    New(nuevoNodo);
    nuevoNodo^.info := valor;  // Asignar valor al nodo
    nuevoNodo^.sig := nil;     // El siguiente nodo es nil

    // Si la lista está vacía, el nuevo nodo es el primero
    if lista = nil then
        lista := nuevoNodo
    else
    begin
        // Recorrer hasta el último nodo
        aux := lista;
        while aux^.sig <> nil do
            aux := aux^.sig;

        // Agregar el nuevo nodo al final
        aux^.sig := nuevoNodo;
    end;
end;

// Extrae los elementos de la lista y los guarda en un archivo
procedure GuardarListaEnArchivo(lista: Tlista; nombreArchivo: string);
var
    archivo: TextFile;
    aux: Tlista;
begin
    // Crear y abrir el archivo para escritura
    Assign(archivo, nombreArchivo);
    Rewrite(archivo);

    // Recorrer la lista y escribir cada elemento en el archivo
    aux := lista;
    while aux <> nil do
    begin
        WriteLn(archivo, aux^.info);  // Escribir el nodo actual
        aux := aux^.sig;             // Avanzar al siguiente nodo
    end;

    // Cerrar el archivo
    Close(archivo);
end;

//#=========== Generación de Datos ===================

// Genera un número aleatorio dentro de un rango
function GenerarNumeroAleatorio(min, max: Integer): Integer;
begin
    GenerarNumeroAleatorio := Random(max - min + 1) + min;  // Fórmula para generar números aleatorios
end;

// Crea una cadena representando un proceso con datos aleatorios
function CrearProcesoAleatorio(indice: Integer): string;
var
    nombre: string;
    instanteArribo, duracion, memoria: Integer;
begin
    // Formato del nombre del proceso: P1, P2, etc.
    nombre := 'P' + IntToStr(indice);
    
    // Generar valores aleatorios para las características del proceso
    instanteArribo := GenerarNumeroAleatorio(1, 100);  // Instante entre 1 y 100
    duracion := GenerarNumeroAleatorio(1, 10);         // Duración entre 1 y 10
    memoria := GenerarNumeroAleatorio(10, 500);        // Memoria entre 10 y 500

    // Combinar los datos en una cadena separada por comas
    CrearProcesoAleatorio := nombre + ',' + IntToStr(instanteArribo) + ',' + IntToStr(duracion) + ',' + IntToStr(memoria);
end;

//#=========== Creación de Tandas ====================

// Procedimiento principal para crear una nueva tanda de procesos
procedure CrearNuevaTanda();
var
    listaDeProcesos: Tlista;         // Lista enlazada para almacenar los procesos
    nombreArchivo: string;          // Nombre del archivo donde se guardará la tanda
    proceso, aux: string;           // Variables auxiliares para el proceso
    tandaNumero: LongInt;           // Declarar como LongInt para evitar el error
    cantidadDeProcesos: Integer;    // Números para el manejo de la tanda
    respuesta: string;              // Respuesta del usuario
    i, cantidad: Integer;           // Contadores
    sr: TSearchRec;                 // Variable para buscar archivos en el directorio
    maxNumero: LongInt;             // Número más alto encontrado en los archivos existentes
begin
    // Inicializar la lista enlazada
    InicializarLista(listaDeProcesos);

    // Preguntar al usuario si los datos serán ingresados manualmente o generados automáticamente
    WriteLn('Quieres cargar los datos manualmente o automaticamente?');
    WriteLn('1: Manual');
    WriteLn('2: Automatico');
    Write('Seleccione una opcion: ');
    ReadLn(respuesta);

    if respuesta = '1' then
    begin
        // Modo de entrada manual
        cantidadDeProcesos := 0;  // Inicializar el contador de procesos
        repeat
            // Incrementar el contador de procesos
            Inc(cantidadDeProcesos);

            // Generar el nombre del proceso
            aux := 'P' + IntToStr(cantidadDeProcesos);
            proceso := aux;

            // Solicitar los datos al usuario
            Write('Instante de arribo: ');
            ReadLn(aux);
            proceso := proceso + ',' + aux;

            Write('Duracion total del trabajo: ');
            ReadLn(aux);
            proceso := proceso + ',' + aux;

            Write('Cantidad de memoria requerida: ');
            ReadLn(aux);
            proceso := proceso + ',' + aux;

            // Insertar el proceso en la lista
            InsertarAlFinal(listaDeProcesos, proceso);

            // Preguntar si se desea agregar otro proceso
            Write('Quieres agregar otro proceso a la tanda? (si/no): ');
            ReadLn(respuesta);
        until respuesta = 'no';  // Salir si el usuario responde "no"
    end
    else if respuesta = '2' then
    begin
        // Modo de generación automática
        Write('Ingrese la cantidad de procesos a generar (1-1000): ');
        ReadLn(cantidad);

        // Validar el rango ingresado
        while (cantidad < 1) or (cantidad > 1000) do
        begin
            Write('Por favor ingrese un numero entre 1 y 1000: ');
            ReadLn(cantidad);
        end;

        // Generar procesos aleatorios
        for i := 1 to cantidad do
        begin
            proceso := CrearProcesoAleatorio(i);  // Generar datos aleatorios
            InsertarAlFinal(listaDeProcesos, proceso);
        end;

        cantidadDeProcesos := cantidad;  // Actualizar la cantidad de procesos
    end
    else
    begin
        // Manejo de opción inválida
        WriteLn('Opcion no valida.');
        Exit;  // Salir del procedimiento
    end;

    // Determinar el número de archivo para la tanda
    maxNumero := 0;
    tandaNumero := 0; // Inicializar tandaNumero para evitar el warning
    if FindFirst('file/tandas/*_tanda(*).txt', faAnyFile, sr) = 0 then
    begin
        repeat
            // Extraer el número de la tanda del nombre del archivo
            aux := Copy(sr.Name, 1, Pos('_tanda(', sr.Name) - 1);
            if TryStrToInt(aux, tandaNumero) then
                if tandaNumero > maxNumero then
                    maxNumero := tandaNumero;
        until FindNext(sr) <> 0;
        FindClose(sr);
    end;
    tandaNumero := maxNumero + 1;  // Incrementar el número más alto encontrado

    // Generar el nombre del archivo
    nombreArchivo := Format('file/tandas/%02d_tanda(%d).txt', [tandaNumero, cantidadDeProcesos]);

    // Guardar la lista en el archivo
    GuardarListaEnArchivo(listaDeProcesos, nombreArchivo);

    // Informar al usuario
    WriteLn('La tanda ha sido creada en el archivo: ', nombreArchivo);
    WriteLn('Se han ingresado ', cantidadDeProcesos, ' procesos.');
end;




end.
