# Ejemplos del tutorial

Programas de práctica del curso de COBOL de **Programación Fácil** que dieron origen a Record System. Se conservan como material de referencia y se ajustaron sólo para que compilen con GnuCOBOL 3 (ver `CHANGELOG.md`).

| Programa | Qué muestra |
|---|---|
| `create-files.cbl` | Alta de registros en un archivo secuencial de líneas (`empleados.dat`) |
| `read-files.cbl` | Lectura secuencial con paginado |
| `output-physical.cbl` | Creación de un archivo indexado vacío |
| `create-indexed-file.cbl` | Alta de registros en el archivo indexado |
| `read-indexed-file.cbl` | Lectura secuencial de un archivo indexado |
| `delete-from-indexed.cbl` | Búsqueda por clave y baja en un archivo indexado |

`PHYSICAL-FILE.cpy` y `LOGICAL-FILE.cpy` son los copybooks que comparten los programas indexados. No estaban en el repositorio original y se reconstruyeron a partir de `read-indexed-file.cbl`.

> Son programas didácticos: no validan la entrada y `create-files` / `create-indexed-file` entran en un bucle si la entrada estándar se corta (por ejemplo, al redirigir un archivo). `read-indexed-file` requiere ejecutar antes `output-physical`.

## Uso

```bash
make examples                 # compila en bin/examples/
cd examples/tutorial
../../bin/examples/read-files # lee empleados.dat (25 registros de ejemplo)
../../bin/examples/output-physical      # crea empleados-indexado.dat
../../bin/examples/create-indexed-file
../../bin/examples/read-indexed-file
```
