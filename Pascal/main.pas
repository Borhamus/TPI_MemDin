program main;
{$codepage UTF8}
//#=========== Dependencias ===========================
// Importamos módulos necesarios para el programa
uses 
    SysUtils,                       // Para manejo de archivos y funciones del sistema
    Process,                        // Para trabajar con procesos
    Crt,                            // Para limpiar pantalla y funciones de consola
    Ucreaciondetanda,               // Módulo propio para manejar vizualizacion de tandas
    Uvizualizadordetandas,          // Módulo propio para manejar creación de tandas
    Uvizualizadordesimulaciones,   // Módulo propio para manejar vizualizacion de Simulaciones
    Usimulador;                     // Módulo propio para manejar creacion de simulaciones

//#=========== Utilidades generales ===================

// Procedimiento para limpiar la consola
procedure LimpiarConsola;
begin
  ClrScr;  // Limpia la consola
end;

// Procedimiento para crear carpetas necesarias para los archivos
procedure CrearCarpetas;
begin
    // Crear carpeta base
    if not DirectoryExists('file') then
        if not CreateDir('file') then
        WriteLn('No se pudo crear la carpeta "file"');

    // Crear subcarpetas
    if not DirectoryExists('file/tandas') then
        if not CreateDir('file/tandas') then
        WriteLn('No se pudo crear la carpeta "file/tandas"');

    if not DirectoryExists('file/simulaciones') then
        if not CreateDir('file/simulaciones') then
        WriteLn('No se pudo crear la carpeta "file/simulaciones"');
end;

// Procedimiento para esperar entrada del usuario antes de continuar
procedure EsperarEntradaUsuario;
begin
    WriteLn; // Salto de línea para separar visualmente
    WriteLn('Presione Enter para continuar...');
    ReadLn;  // Espera a que el usuario presione Enter
    LimpiarConsola;  // Limpia la consola después de la pausa
end;

//#=========== Manejo del menú ========================

// Procedimiento que muestra el menú principal del programa
procedure MostrarMenu;
var 
    Opcion: string;  // Almacena la opción seleccionada por el usuario
begin
    repeat
        LimpiarConsola;  // Limpia la pantalla antes de mostrar el menú
        // Título e introducción del programa
        WriteLn('================================================');
        WriteLn('Bienvenido al programa de simulacion de memoria');
        WriteLn('Este programa simula la asignacion de memoria en un sistema multiprogramado y mono-procesador.');
        WriteLn('Se implementan estrategias como First-Fit, Best-Fit, Next-Fit, y Worst-Fit.');
        WriteLn('El objetivo es estudiar el comportamiento de distintas estrategias de administracion de memoria.');
        WriteLn('================================================');
        WriteLn('Desarrollado por: Franco Joaquin Gomez');
        WriteLn('Materia: Sistemas Operativos');
        WriteLn('================================================');
        
        // Opciones del menú
        WriteLn('1) Crear Nueva Tanda');
        WriteLn('2) Ver archivos de tandas creadas');
        WriteLn('3) Realizar Simulacion');
        WriteLn('4) Ver Simulaciones realizadas');    
        WriteLn('5) Salir');
        WriteLn('');
        Write('Seleccione una opcion (1-5): ');
        ReadLn(Opcion);  // Lee la opción del usuario

        // Estructura de decisión para manejar las opciones del menú
        case Opcion of
            '1':begin
                    // Opción para crear una nueva tanda
                    EsperarEntradaUsuario;  // Pausa antes de comenzar
                    CrearNuevaTanda();      // Llama al procedimiento definido en `creaciondetanda`
                end;
            '2':begin
                    // Opción para mostrar archivos de tandas creadas
                    EsperarEntradaUsuario;
                    WriteLn('Ver archivos de tandas creadas:');
                    MostrarArchivosTandas();  // Llama al procedimiento definido en `creaciondetanda`
                end;
            '3':begin
                    // Opción para realizar simulaciones
                    WriteLn('Comenzando los requisitos para la creación de la simulación.');
                    EsperarEntradaUsuario;  // Pausa antes de volver al menú
                    SimularTanda();
                end;
            '4':begin
                    EsperarEntradaUsuario;  
                    MostrarArchivosSimulaciones();
                end;
            '5':WriteLn('Saliendo del programa...');  // Mensaje de salida
        else
            begin
                // Manejo de entrada inválida
                WriteLn('Opción no válida. Por favor, intente nuevamente.');
                EsperarEntradaUsuario;  // Pausa antes de volver al menú
            end;
        end;
    until Opcion = '5';  // Repite el menú hasta que el usuario elija salir
end;

//#=========== Programa principal =====================

// Punto de entrada del programa
begin
    CrearCarpetas;  // Asegura que la carpeta "files" esté disponible
    MostrarMenu;    // Muestra el menú principal al usuario
end.
