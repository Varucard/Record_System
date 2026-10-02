# Changelog

Todos los cambios relevantes del proyecto se documentan en este archivo.
El formato se basa en [Keep a Changelog](https://keepachangelog.com/es-ES/1.1.0/) y el proyecto usa [versionado semántico](https://semver.org/lang/es/).

## [2.0.0] - 2026-10-02

### Agregado
- Menú principal con los módulos Clientes, Equipos, Presupuestos y pagos, y Reportes.
- Archivos de datos indexados (`customers.dat`, `equipments.dat`, `budgets.dat`) con claves alternativas por cliente y por equipo.
- Verificación de los archivos al iniciar: se crean si no existen y se informan los errores de apertura.
- ABM completo de clientes, equipos y presupuestos.
- Estados de reparación de equipos y registro de pagos con forma y fecha de pago.
- Reporte general: equipos por estado, montos cobrados y pendientes, recaudación por forma de pago.
- Validaciones de DNI, email, importes y fechas, e integridad referencial al eliminar.
- Directorio de datos configurable (`RS_DATA_DIR`), sin rutas absolutas de Windows.
- `Makefile`, `Dockerfile` multi-etapa, `docker-compose.yml` y CI con GitHub Actions.
- Pruebas de punta a punta y chequeo de formato COBOL.
- Documentación: README, arquitectura y guía de contribución.
- Licencia MIT.
- Lanzador `scripts/record-system.sh`: crea el directorio de datos, verifica permisos e impide abrir dos instancias sobre los mismos datos (`flock`).
- Objetivo `make backup`.
- Importes en formato argentino (`15.000,50`) con `DECIMAL-POINT IS COMMA`.
- Paginado de listados con opción de cortar (`0`).
- En las modificaciones, `-` borra un dato opcional.
- Confirmación al entregar un equipo con deuda o al presupuestar un equipo entregado.
- Copybooks `PHYSICAL-FILE` y `LOGICAL-FILE` que faltaban para compilar los ejemplos indexados del tutorial.

### Cambiado
- El código se reorganizó en `src/` (programa principal + subprogramas) y `src/copybooks/`.
- El DNI pasa a ser la clave primaria de clientes. Los equipos y presupuestos usan un número autonumérico.
- Los programas del curso pasaron de `original_code/` a `examples/tutorial/`.
- Codificación UTF-8 y finales de línea LF en todo el repositorio.

### Corregido
- `READ-FILES`: se eliminó la sentencia inválida `DISPLAY ADD MUESTRA-ID TO "1"`.
- `OUTPUT-PHYSICAL`: `IDENTIFICATION DIVISION` empezaba en la columna del indicador.
- `READ-INDEXED-FILE`: `PROGRAM-ID` (`CAPITULO-27`) e indentación.
- Textos con acentos que no entraban en su `PIC`.
- Se dejaron de versionar los binarios (`.exe`) y los archivos de datos.
- Hallazgos de la revisión de código previa a la publicación:
  - Pérdida de datos con dos instancias abiertas a la vez.
  - "15.000" se interpretaba como $15.
  - El DNI con y sin cero a la izquierda generaba clientes duplicados.
  - Desborde del autonumérico al llegar a 99999.
  - Datos que se guardaban cortados sin aviso.
  - Las entradas con CRLF no se reconocían.
  - Posibles bucles infinitos ante errores de E/S.
  - La pausa del paginado aparecía de más.
  - La confirmación aceptaba cualquier palabra que empezara con "s".
  - El email se validaba de forma débil.
  - La fecha de pago podía ser anterior a la del presupuesto.
  - Se agrandaron los campos editados de totales.

## [1.0.0] - 2023-02-05

### Agregado
- Primera versión: alta secuencial de clientes y definición de los archivos de equipos y presupuestos.
- Programas de práctica del curso de COBOL (`original_code/`).
