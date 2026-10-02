# Guía de contribución

## Flujo de ramas

| Rama | Uso |
|---|---|
| `main` | Versión estable. Sólo recibe merges desde `develop` (o hotfixes). |
| `develop` | Integración de funcionalidades terminadas. |
| `feature/<descripcion>` | Una rama por funcionalidad, creada desde `develop`. |
| `fix/<descripcion>` | Correcciones. |

1. Crear la rama desde `develop`: `git switch -c feature/mi-cambio develop`.
2. Hacer commits pequeños, con mensajes en imperativo (por ejemplo, "Agrega listado de equipos por estado").
3. Antes de abrir el pull request, correr `make test` (o `make docker-test`).
4. Abrir el pull request hacia `develop` y actualizar el `CHANGELOG.md`.

## Estilo de código

- COBOL en formato fijo: el código va de la columna 8 a la 72. `make lint` lo verifica.
- UTF-8, LF, sin tabulaciones (ver `.editorconfig`).
- Usar terminadores explícitos (`END-IF`, `END-READ`, `END-DISPLAY`…).
- Las definiciones de archivos van en `src/copybooks/` (`fc-*.cpy` y `fd-*.cpy`) y se reutilizan con `COPY`.
- Toda operación de archivo indexado debe manejar `INVALID KEY` / `AT END` y su `FILE STATUS`.
- Un módulo nuevo se registra en `SOURCES` del `Makefile` y, si usa `proc-comun.cpy`, define `CERRAR-ARCHIVOS`.

## Pruebas

Cada funcionalidad nueva debe tener un caso en `tests/run_tests.sh`. Los casos simulan la entrada del usuario línea por línea:

```bash
start_case "descripción del caso"
run_system 1 3 0 0            # opciones y datos que tipearía el usuario
expect "texto esperado"
expect_not "texto que no debe aparecer"
end_case
```

## Cambios en el formato de registros

Si se modifica un `fd-*.cpy`, los `.dat` existentes dejan de ser compatibles. En ese caso hay que documentarlo en el `CHANGELOG.md` como cambio incompatible y subir la versión mayor.
