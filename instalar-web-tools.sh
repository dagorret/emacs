#!/usr/bin/env bash
set -e

echo "=== Instalador de herramientas web para Emacs (LSP + JS) ==="

#---------------------------------------
# 1) Node.js + npm
#---------------------------------------
if command -v node >/dev/null 2>&1 && command -v npm >/dev/null 2>&1; then
  echo "[OK] node y npm ya están instalados."
else
  echo "[INFO] node o npm no encontrados. Instalando con apt..."
  sudo apt update
  sudo apt install -y nodejs npm
fi

echo
echo "Versiones:"
node -v || echo "node no disponible"
npm -v  || echo "npm no disponible"

#---------------------------------------
# Función auxiliar: instalar paquete npm -g si falta
#---------------------------------------
instalar_npm_global() {
  local pkg="$1"
  if npm list -g --depth=0 "$pkg" >/dev/null 2>&1; then
    echo "[OK] $pkg ya está instalado globalmente."
  else
    echo "[INSTALANDO] $pkg (npm -g install)..."
    sudo npm install -g "$pkg"
  fi
}

echo
echo "=== Instalando paquetes npm globales ==="

# 2) Servidores LSP estilo VSCode (CSS/HTML/JSON)
instalar_npm_global "vscode-langservers-extracted"

# 3) Tailwind CSS language server (por si lo usas en algún IDE o más adelante)
instalar_npm_global "@tailwindcss/language-server"

# 4) Prettier (autoformateo)
instalar_npm_global "prettier"

# 5) ESLint (lint JS/TS)
instalar_npm_global "eslint"

# 6) Stylelint (lint CSS/SCSS, por si lo usas desde terminal)
instalar_npm_global "stylelint"

echo
echo "=== Comprobando ejecutables en PATH ==="

for cmd in \
  vscode-css-language-server \
  vscode-html-language-server \
  vscode-json-language-server \
  tailwindcss-language-server \
  prettier eslint stylelint; do
  if command -v "$cmd" >/dev/null 2>&1; then
    echo "[OK] $cmd -> $(command -v "$cmd")"
  else
    echo "[AVISO] $cmd no se encontró en PATH (revisa ~/.npm-global o similar)."
  fi
done

echo
echo "=== Listo. Herramientas web instaladas. ==="

