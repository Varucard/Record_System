# Record System

[![CI](https://github.com/Varucard/Record_System/actions/workflows/ci.yml/badge.svg)](https://github.com/Varucard/Record_System/actions/workflows/ci.yml)
[![Licencia: MIT](https://img.shields.io/badge/licencia-MIT-blue.svg)](LICENSE)

Sistema de registros para un **servicio técnico de equipos informáticos**, escrito en **COBOL** (GnuCOBOL).

Permite gestionar clientes, los equipos que ingresan a reparación, los presupuestos de cada trabajo y sus pagos. Además emite comprobantes y exporta los datos a Excel. Los datos se guardan en archivos `.dat` **indexados**, y el sistema crea y verifica esos archivos al iniciar.

Tiene dos interfaces: la **clásica**, de terminal a la antigua (la de siempre, por defecto), y un **modo pantalla** opcional, con pantalla completa, recuadros y colores (incluido un tema de fósforo verde estilo mainframe).

```
======================================================================
  MENÚ PRINCIPAL
======================================================================
  1. Clientes
  2. Equipos
  3. Presupuestos y pagos
  4. Reportes
  0. Salir
Opción:
```

## Funcionalidades

| Módulo | Operaciones |
|---|---|
| **Clientes** | Alta, consulta (con sus equipos), listado, búsqueda por parte del nombre, modificación y baja. |
| **Equipos** | Ingreso con número automático, consulta (con sus presupuestos), listado general y por cliente, modificación, cambio de estado (*Ingresado*, *En reparación*, *Listo para retirar*, *Entregado*), baja y **comprobante de ingreso** para el cliente. |
| **Presupuestos y pagos** | Alta asociada a un equipo, consulta, listado general, por equipo y pendientes de pago, registro del pago (forma y fecha), modificación, baja, **estado de cuenta por cliente** (saldo adeudado) y **comprobante** de presupuesto o recibo de pago. |
| **Reportes** | Reporte general (clientes, equipos por estado, presupuestos pagados y pendientes, monto cobrado y adeudado, recaudación por forma de pago) y **exportación a CSV** para Excel. |

Reglas de negocio:

- El DNI identifica al cliente: tiene 7 u 8 dígitos, se guarda con 8 (`1234567` = `01234567`) y no puede repetirse.
- Un equipo sólo se puede registrar para un cliente existente, y un presupuesto sólo para un equipo existente.
- No se puede eliminar un cliente que tiene equipos, ni un equipo que tiene presupuestos.
- Un presupuesto pagado no se puede modificar ni eliminar. La fecha de pago no puede ser anterior a la del presupuesto.
- El estado de un equipo se puede cambiar libremente (para corregir errores). Al marcarlo como *Entregado* con presupuestos impagos, o al presupuestar un equipo ya entregado, el sistema pide confirmación.
- Los importes usan el formato argentino: `15.000,50` (también se acepta `15000.50`). Se muestran como `$ 15.000,50`.
- Se validan el email, las fechas (`DD/MM/AAAA`, con ENTER se toma la fecha de hoy) y el largo de cada dato, para que nada se guarde cortado.
- En las modificaciones, ENTER mantiene el valor actual y `-` borra un dato opcional. En las búsquedas, ENTER cancela la operación.
- Escribir `*` en cualquier dato cancela el alta o la modificación en curso sin grabar nada.
- Los números de equipo y de presupuesto nunca se reutilizan, aunque se borre el último (`control.dat`).
- Los listados se paginan cada 20 renglones: ENTER continúa y `0` termina el listado.

## Interfaces

| Interfaz | Cómo se activa | Descripción |
|---|---|---|
| **Clásica** (por defecto) | — | Terminal de línea a la antigua: menús numerados, todo queda en pantalla. Funciona en cualquier terminal y permite automatizar con entrada redirigida. |
| **Pantalla** | `--pantalla` o `RS_INTERFAZ=pantalla` | Pantalla completa: limpia la pantalla entre menús, barra superior con fecha y hora, títulos en recuadros con color y pausa para leer cada resultado. |
| **Verde** | `--verde` o `RS_TEMA=verde` | Modo pantalla con tema de fósforo verde, estilo terminal de mainframe. |

```bash
make run                       # clásica
make run INTERFAZ=pantalla     # pantalla completa
make run INTERFAZ=verde        # pantalla completa, fósforo verde
docker compose run --rm record-system --verde
```

## Comprobantes y exportación

- **Comprobantes**: se generan como texto listo para imprimir en `data/comprobantes/`:
  - `ingreso-NNNNN.txt`: ingreso del equipo, con datos del cliente y firma;
  - `presupuesto-NNNNN.txt`: presupuesto pendiente;
  - `recibo-NNNNN.txt`: recibo, si el presupuesto ya está pagado.

  El nombre del negocio se toma de `RS_EMPRESA`.
- **Exportación CSV** (Reportes → 2): genera `clientes.csv`, `equipos.csv` y `presupuestos.csv` en `data/exportes/`, con separador `;` y codificación UTF-8 con BOM, para abrirlos directamente en Excel con acentos e importes correctos.

## Inicio rápido con Docker (recomendado)

No hace falta instalar nada salvo [Docker](https://docs.docker.com/get-docker/).

```bash
docker compose run --rm record-system
```

Los datos quedan en la carpeta `data/` del proyecto, así que persisten entre ejecuciones.

El contenedor corre con el UID:GID `1000:1000`, que debe poder escribir en `data/`. Si su usuario tiene otro UID:

```bash
UID=$(id -u) GID=$(id -g) docker compose run --rm record-system
```

Otros comandos:

```bash
make docker-build   # construye la imagen
make docker-run     # construye y ejecuta
make docker-test    # compila y corre las pruebas dentro de Docker
```

## Compilación local

Requisitos: [GnuCOBOL](https://gnucobol.sourceforge.io/) 3.x con soporte de archivos indexados, `make` y `bash`.

```bash
# Debian / Ubuntu / WSL
sudo apt install gnucobol3 make

make build    # genera bin/record_system
make run      # ejecuta usando ./data como directorio de datos
make test     # chequeo de formato + pruebas de punta a punta
make backup   # copia los datos a backups/<fecha-hora>/
make help     # lista todos los objetivos
```

En Windows se puede usar WSL, Docker o [OpenCobolIDE](https://github.com/OpenCobolIDE/OpenCobolIDE). Para compilar a mano:

```bash
cobc -x -std=default -fstatic-call -I src/copybooks -o bin/record_system \
     src/record_system.cbl src/verificar_archivos.cbl src/clientes.cbl \
     src/equipos.cbl src/presupuestos.cbl src/reportes.cbl
```

## Un solo usuario a la vez

Los archivos indexados no admiten que dos instancias escriban a la vez: los registros de una de ellas se perderían. Por eso el sistema se inicia con el lanzador `scripts/record-system.sh` (lo usan `make run` y la imagen Docker), que:

- crea el directorio de datos si no existe (incluidas las carpetas intermedias);
- verifica que se pueda escribir en él;
- bloquea los datos con `flock`: si ya hay una instancia abierta, la segunda termina con un aviso (código de salida 75).

Si se ejecuta `bin/record_system` directamente, este control no se aplica.

## Respaldos

Cada archivo `.dat` tiene índices asociados (`.dat.1`, `.dat.2`) que deben copiarse junto con él, y `control.dat` guarda la numeración. `make backup` copia todos los `.dat*` a `backups/<fecha-hora>/`. Para restaurar, copie esos archivos de vuelta a `data/` con el sistema cerrado.

## Configuración

| Variable | Descripción | Valor por defecto |
|---|---|---|
| `RS_DATA_DIR` | Directorio donde se guardan los archivos `.dat`. El lanzador lo crea si no existe. | `data` |
| `RS_BIN` | Ejecutable que inicia el lanzador. | `record_system` del `PATH` |
| `RS_INTERFAZ` | `clasica` o `pantalla`. | `clasica` |
| `RS_TEMA` | Tema del modo pantalla: `color` o `verde`. | `color` |
| `RS_EMPRESA` | Nombre del negocio en los comprobantes. | `Servicio Técnico` |
| `RS_FECHA_HOY` | Fija la fecha "de hoy" (`AAAAMMDD`). Se usa en las pruebas. | fecha del sistema |
| `TZ` | Zona horaria (Docker). | `America/Argentina/Buenos_Aires` |

## Estructura del proyecto

```
.
├── src/                    Código fuente del sistema
│   ├── record_system.cbl   Programa principal: inicio y menú
│   ├── verificar_archivos.cbl
│   ├── clientes.cbl
│   ├── equipos.cbl
│   ├── presupuestos.cbl
│   ├── reportes.cbl
│   └── copybooks/          Definiciones de archivos y rutinas compartidas
├── scripts/                Lanzador (directorio de datos e instancia única)
├── tests/                  Pruebas automatizadas y chequeo de formato
├── examples/tutorial/      Programas del curso que dieron origen al sistema
├── docs/                   Documentación técnica
├── data/                   Archivos de datos (no se versionan)
├── Dockerfile
├── docker-compose.yml
└── Makefile
```

En [docs/ARQUITECTURA.md](docs/ARQUITECTURA.md) se describen los módulos, el diseño de los archivos y sus registros.

## Pruebas

`make test` (o `make docker-test`) ejecuta:

1. **Chequeo de formato** (`tests/check_format.sh`): ninguna línea supera la columna 72, sin tabulaciones ni CRLF.
2. **Pruebas de punta a punta** (`tests/run_tests.sh`): ejecutan el sistema con una entrada simulada sobre un directorio temporal y verifican la salida. Cubren:
   - altas, validaciones (DNI, email, importes, fechas, largos), cancelación con `*` y persistencia;
   - numeración sin reutilizar números, estados, pagos e integridad al eliminar;
   - búsqueda, estado de cuenta, paginado, reportes, comprobantes y CSV;
   - textos con acentos, entradas con CRLF, ambas interfaces y el bloqueo de instancia única.

La integración continua (GitHub Actions) corre estas mismas pruebas en cada push y pull request.

## Contribuir

Ver [CONTRIBUTING.md](CONTRIBUTING.md). Los cambios por versión se registran en [CHANGELOG.md](CHANGELOG.md).

## Licencia

Distribuido bajo la licencia [MIT](LICENSE): software libre, se puede usar, modificar y redistribuir conservando el aviso de copyright.

## Herramientas

COBOL (GnuCOBOL 3), Visual Studio Code, OpenCobolIDE, Docker.

## Agradecimientos

Al canal de YouTube **"Programación Fácil"**: gracias a su curso de COBOL y a su código este programa pudo ser diseñado. Los programas originales del curso se conservan en [examples/tutorial](examples/tutorial).
