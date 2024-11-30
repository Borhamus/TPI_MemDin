unit Uvizualizadordetandas;
{$codepage UTF8}

interface

uses
  SysUtils;  // Necesario para manejo de archivos y cadenas

procedure MostrarArchivosTandas();

implementation

//#=========== Mostrar archivos de tandas ===========================
procedure MostrarArchivosTandas();
var
  Info: TSearchRec;
  Archivos: array of string;
  i, opcion: Integer;
  archivo: TextFile;
  linea: string;
begin

  //#=========== Buscar archivos en "file/tandas" ====================
  // Buscar archivos con extensión .txt en la carpeta "file/tandas"
  if FindFirst('file/tandas/*.txt', faAnyFile, Info) = 0 then
  begin
    i := 0;
    WriteLn('Lista de tandas disponibles:');
    
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
      WriteLn('Has seleccionado: ', Archivos[opcion - 1]);
      
      //#=========== Abrir el archivo seleccionado ======================
      // Abrir el archivo seleccionado en modo lectura
      Assign(archivo, 'file/tandas/' + Archivos[opcion - 1]);
      Reset(archivo);  // Abrir archivo para leer

      //#=========== Mostrar contenido del archivo ======================
      WriteLn('Contenido del archivo:');
      WriteLn('Nombre del Proceso// Instante de Arribo// Duracion total// Cantidad de memoria requerida:');
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
    WriteLn('No hay archivos de tandas disponibles.');

  //#=========== Regresar al menú ================================
  WriteLn();
  WriteLn('Presione Enter para regresar al menú.');
  ReadLn;
end;

end.
