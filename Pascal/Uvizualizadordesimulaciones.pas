unit Uvizualizadordesimulaciones;
{$codepage UTF8}

interface

uses
    SysUtils, Crt;

procedure MostrarArchivosSimulaciones();

implementation

//#=========== Mostrar archivos de simulaciones ====================
procedure MostrarArchivosSimulaciones();
var
    Info: TSearchRec;
    Archivos: array of string;
    i, opcion: Integer;
    archivo: TextFile;
    linea: string;
begin
    // Limpiar la pantalla
    ClrScr;

  //#=========== Buscar archivos en "file/simulaciones" =============
  // Buscar archivos con extensión .txt en la carpeta "file/simulaciones"
    if FindFirst('file/simulaciones/*.txt', faAnyFile, Info) = 0 then
    begin
    i := 0;
    WriteLn('Lista de simulaciones realizadas:');
    
    //#=========== Mostrar lista de archivos disponibles ==============
    // Recorrer los archivos encontrados y mostrarlos en pantalla
    repeat
        Inc(i);
        SetLength(Archivos, i);
      Archivos[i - 1] := Info.Name;  // Guardar el nombre del archivo
      WriteLn(i, ') ', Info.Name);   // Mostrar el índice y el nombre del archivo
    until FindNext(Info) <> 0;
    FindClose(Info);

    //#=========== Selección de archivo ===============================
    // Permitir al usuario seleccionar un archivo
    WriteLn;
    Write('Seleccione un archivo (1-', i, '): ');
    ReadLn(opcion);

    if (opcion >= 1) and (opcion <= i) then
    begin
      // Limpiar la pantalla
        ClrScr;
        WriteLn('Has seleccionado: ', Archivos[opcion - 1]);
        
      //#=========== Abrir el archivo seleccionado ======================
      // Abrir el archivo seleccionado en modo lectura
        Assign(archivo, 'file/simulaciones/' + Archivos[opcion - 1]);
        Reset(archivo);  // Abrir archivo para leer

      //#=========== Mostrar contenido del archivo ======================
        WriteLn('Contenido del archivo:');
      // Leer el archivo línea por línea y mostrarlo en pantalla
        while not Eof(archivo) do
        begin
            ReadLn(archivo, linea);  // Leer una línea del archivo
            WriteLn(linea);          // Mostrar la línea leída
        end;

      //#=========== Cerrar archivo ====================================
      // Cerrar el archivo después de leer su contenido
        Close(archivo);
    end
    else
        WriteLn('Opción no válida.');
    end
    else
    WriteLn('No hay simulaciones realizadas.');

  //#=========== Regresar al menú ================================
    WriteLn('Presione Enter para regresar al menú.');
    ReadLn;
end;

end.
