#!/usr/bin/env bash
# Verifica reglas de formato del código COBOL (formato fijo):
#  - ninguna línea supera la columna 72 (el compilador ignora el resto
#    de la línea sin avisar, lo que provoca errores difíciles de ver);
#  - sin tabulaciones ni finales de línea CRLF.
#
# Uso: tests/check_format.sh [directorio ...]

set -u
status=0

for dir in "${@:-src}"; do
    if [[ ! -d "$dir" ]]; then
        echo "No existe el directorio '$dir'."
        exit 2
    fi
done

while IFS= read -r -d '' file; do
    if LC_ALL=C grep -n $'\r' "$file" >/dev/null; then
        echo "$file: contiene finales de línea CRLF"
        status=1
    fi
    if LC_ALL=C grep -n $'\t' "$file" >/dev/null; then
        echo "$file: contiene tabulaciones"
        status=1
    fi
    LC_ALL=C awk -v f="$file" \
        'length($0) > 72 { printf "%s:%d: supera la columna 72 (%d)\n", f, FNR, length($0); bad = 1 }
         END { exit bad }' "$file" || status=1
done < <(find "${@:-src}" -type f \( -name '*.cbl' -o -name '*.cpy' \) -print0)

if [[ "$status" -eq 0 ]]; then
    echo "Formato OK."
fi
exit "$status"
