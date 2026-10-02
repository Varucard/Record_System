# Arquitectura

## Arranque

`scripts/record-system.sh` prepara el directorio de datos, toma un bloqueo exclusivo (`flock`) sobre `<datos>/.record_system.lock` y ejecuta el binario. El bloqueo existe porque GnuCOBOL con Berkeley DB no comparte el estado entre procesos: dos instancias que escriben el mismo archivo pisan los registros de la otra.

## Módulos

El sistema se compila en un único ejecutable compuesto por un programa principal y subprogramas que se invocan con `CALL`. Todos los módulos reciben el directorio de datos (`LK-DIRECTORIO-DATOS`).

```
RECORD-SYSTEM (record_system.cbl)
│  Pantalla de inicio, lee RS_DATA_DIR, crea el directorio y muestra el menú
│
├── VERIFICAR-ARCHIVOS (verificar_archivos.cbl)
│      Abre cada .dat en modo I-O: lo crea si no existe e informa errores
├── CLIENTES      (clientes.cbl)      customers.dat I-O, equipments.dat INPUT
├── EQUIPOS       (equipos.cbl)       equipments.dat y control.dat I-O, customers.dat y budgets.dat INPUT
├── PRESUPUESTOS  (presupuestos.cbl)  budgets.dat y control.dat I-O, customers.dat y equipments.dat INPUT
└── REPORTES      (reportes.cbl)      los tres archivos INPUT; genera los CSV
```

EQUIPOS y PRESUPUESTOS usan `control.dat` para numerar y generan comprobantes en `<datos>/comprobantes/`.

Cada módulo abre sus archivos al entrar y los cierra al volver al menú principal. Los archivos que sólo consulta (para validar existencia o integridad) los abre como `INPUT`.

## Copybooks (`src/copybooks`)

| Copybook | Contenido |
|---|---|
| `fc-*.cpy` | `SELECT` de cada archivo (FILE-CONTROL) |
| `fd-*.cpy` | `FD` y registro de cada archivo (FILE SECTION) |
| `ws-archivos.cpy` | Rutas dinámicas y `FILE STATUS` de los archivos |
| `ws-comun.cpy` | Variables de trabajo compartidas (entrada, flags, fechas, importes) |
| `lk-comun.cpy` | Parámetro `LK-DIRECTORIO-DATOS` |
| `proc-rutas.cpy` | Párrafo `ARMAR-RUTAS` |
| `fc-control.cpy` / `fd-control.cpy` / `proc-control.cpy` | Archivo de control y párrafos `AJUSTAR-ID-CONTROL` / `REGISTRAR-ID-CONTROL` |
| `fc-texto.cpy` / `fd-texto.cpy` / `proc-texto.cpy` | Archivo de texto de salida; apertura en un subdirectorio de datos, comprobantes (encabezado, datos alineados, pie con firma) |
| `proc-comun.cpy` | Rutinas de entrada y validación: `LEER-ENTRADA`, `LEER-DNI`, `LEER-ID`, `LEER-TEXTO`, `LEER-TEXTO-OBLIGATORIO` (respetan `WS-LARGO-MAXIMO`), `ACTUALIZAR-CAMPO`, `LEER-EMAIL`, `LEER-IMPORTE`, `LEER-FECHA` (con `WS-FECHA-MINIMA` opcional), `CONFIRMAR`, `FORMATEAR-FECHA`, `CONTROLAR-PAGINA` |

`proc-comun.cpy` también contiene:

- `AJUSTAR-ANCHO` / `AGREGAR-COLUMNA`: alinean columnas contando caracteres y no bytes, y recortan sin partir caracteres UTF-8 multibyte.
- La interfaz: `INICIAR-INTERFAZ`, `PREPARAR-PANTALLA`, `MOSTRAR-TITULO`, `MARCAR-PAUSA` y `RESTAURAR-TERMINAL`. En modo clásico se comportan como siempre; en modo pantalla usan secuencias ANSI.
- `VERIFICAR-CANCELACION`: un `*` marca `OPERACION-CANCELADA`, las lecturas siguientes no preguntan nada y quien llama no graba.

Todos los programas declaran `DECIMAL-POINT IS COMMA`: los importes se leen y muestran en formato argentino (`15.000,50`).

Un programa que incluye `proc-comun.cpy` debe definir el párrafo `CERRAR-ARCHIVOS`. `LEER-ENTRADA` lo ejecuta antes de terminar cuando se acaba la entrada estándar (por ejemplo, al redirigir un archivo), así los archivos quedan cerrados correctamente y el programa termina con código 2.

## Archivos de datos

Todos son `ORGANIZATION IS INDEXED`, `ACCESS MODE IS DYNAMIC`, con rutas asignadas en tiempo de ejecución (`ASSIGN DYNAMIC`). GnuCOBOL usa Berkeley DB: cada clave alternativa se guarda en un archivo adicional (`budgets.dat.1`, `budgets.dat.2`, etc.), que hay que respaldar junto con el principal.

### customers.dat: clientes (161 bytes)

| Campo | PIC | Descripción |
|---|---|---|
| `CUSTOMERS-DNI` | `X(8)` | **Clave primaria.** 8 dígitos, con cero a la izquierda si el DNI tiene 7 |
| `CUSTOMERS-NAME` | `X(40)` | Nombre y apellido (obligatorio) |
| `CUSTOMERS-CELLPHONE` | `X(15)` | Teléfono |
| `CUSTOMERS-EMAIL` | `X(50)` | Email (validado si se informa) |
| `CUSTOMERS-ADDRESS` | `X(40)` | Dirección |
| `CUSTOMERS-FECHA-ALTA` | `9(8)` | AAAAMMDD |

### equipments.dat: equipos (337 bytes)

| Campo | PIC | Descripción |
|---|---|---|
| `EQUIPMENTS-ID` | `9(5)` | **Clave primaria.** Autonumérico (último + 1, máximo 99999) |
| `EQUIPMENTS-DNI` | `X(8)` | **Clave alternativa con duplicados.** Cliente dueño |
| `EQUIPMENTS-TIPO` | `X(15)` | PC, Notebook, Impresora… |
| `EQUIPMENTS-DESCRIPCION` | `X(100)` | Marca / modelo |
| `EQUIPMENTS-CARACTERISTICAS` | `X(100)` | Opcional |
| `EQUIPMENTS-PROBLEMA` | `X(100)` | Falla informada |
| `EQUIPMENTS-ESTADO` | `X(1)` | `I` Ingresado, `R` En reparación, `L` Listo para retirar, `E` Entregado |
| `EQUIPMENTS-FECHA-INGRESO` | `9(8)` | AAAAMMDD |

### budgets.dat: presupuestos (161 bytes)

| Campo | PIC | Descripción |
|---|---|---|
| `BUDGETS-ID` | `9(5)` | **Clave primaria.** Autonumérico |
| `BUDGETS-EQUIPO-ID` | `9(5)` | **Clave alternativa con duplicados.** Equipo |
| `BUDGETS-DNI` | `X(8)` | **Clave alternativa con duplicados.** Cliente |
| `BUDGETS-DESCRIPCION` | `X(100)` | Trabajo a realizar |
| `BUDGETS-IMPORTE` | `9(9)V99` | Importe |
| `BUDGETS-FORMA-PAGO` | `X(15)` | Efectivo, Débito, Crédito, Transferencia, Mercado Pago, Otra |
| `BUDGETS-FECHA` | `9(8)` | Fecha del presupuesto |
| `BUDGETS-PAGADO` | `X(1)` | `S` pagado, `N` pendiente |
| `BUDGETS-FECHA-PAGO` | `9(8)` | Fecha de pago (0 si está pendiente) |

### control.dat: numeración (17 bytes)

| Campo | PIC | Descripción |
|---|---|---|
| `CONTROL-CLAVE` | `X(12)` | **Clave primaria.** `EQUIPOS` o `PRESUPUESTOS` |
| `CONTROL-ULTIMO-ID` | `9(5)` | Último número asignado |

El próximo número es el mayor entre "último del archivo + 1" y "último asignado + 1". Así no se repiten números de registros borrados, y los datos anteriores a la versión 2.1 siguen funcionando.

### Relaciones

```
customers (DNI) 1 ──< N equipments (EQUIPMENTS-DNI)
equipments (ID) 1 ──< N budgets    (BUDGETS-EQUIPO-ID)
```

La integridad referencial la controla la aplicación: valida que el padre exista en las altas y no permite eliminar un padre que tenga hijos.

## Convenciones de código

- Formato fijo: el código va de la columna 8 a la 72 (lo verifica `tests/check_format.sh`).
- Fuentes en UTF-8 con finales de línea LF (`.gitattributes`, `.editorconfig`).
- Sentencias con terminadores explícitos (`END-IF`, `END-READ`, `END-DISPLAY`…).
- Nombres de archivos y registros en inglés, como en el diseño original (`CUSTOMERS-*`, `EQUIPMENTS-*`, `BUDGETS-*`); párrafos y variables de trabajo en español (`WS-*`, `LK-*`).
- Entrada y salida en modo línea (`ACCEPT` / `DISPLAY`), lo que permite probar el sistema redirigiendo la entrada estándar.
