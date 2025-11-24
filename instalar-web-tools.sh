#!/usr/bin/env bash
set -e

echo "=== Instalador de herramientas web para Emacs ==="

#---------------------------------------
# 1) Instalar Node.js + npm (si faltan)
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

# 2) Tailwind CSS language server (para lsp-tailwindcss)
instalar_npm_global "@tailwindcss/language-server"

# 3) Prettier (autoformato JS/TS/HTML/CSS)
instalar_npm_global "prettier"

# 4) ESLint (lint JS/TS)
instalar_npm_global "eslint"

# 5) Stylelint (lint CSS/SCSS)
instalar_npm_global "stylelint"

echo
echo "=== Comprobando ejecutables en PATH ==="

for cmd in tailwindcss-language-server prettier eslint stylelint; do
  if command -v "$cmd" >/dev/null 2>&1; then
    echo "[OK] $cmd encontrado en PATH: $(command -v "$cmd")"
  else
    echo "[AVISO] $cmd no se encontró en PATH (revisa ~/.npm-global o similar)."
  fi
done

echo
echo "=== Listo. Ahora Emacs puede usar Tailwind LSP, Prettier, ESLint y Stylelint. ==="

