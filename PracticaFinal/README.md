# Instrucciones para ejecutar

1. Descomprimir fichero
2. Ejecutar una terminal en la carpeta descomprimida
3. Ejecutar './topology.sh <comando de terminal>' para lanzar seis terminales con erlang
4. En el primer terminal de erlang compilar los módulos 'comp' y 'testnet'
5. Ejecutar comp:compile().
6. Ejecutar testnet:start().

## El programa tiene dos modos

1. Cliente ligero - Lanzado desde el comando ddb:init() o ddb:start(light)
   - Cliente sin interfaz que sólo guarda datos.
   - Si se desea lanzar la interfaz, ejecutar interface:start().

2 Cliente completo - Lanzado desde el comando ddb:full() o ddb:start(full).
  - Cliente con interfaz para el usuario.
  - Todas las operaciones son transparentes para éste
  
