# TPErlang-Final

# Protocolo de inicio:
1. Asegurarse que no haya ningun .beam
2. make
3. en una terminal de las generadas ejecutar make:all().
4. en la misma terminal ejecutar conectar:con(X). donde X es el número de terminales, debe coincidir con NODOS en makefile
5. ejecutar node:start(). en todas las terminales
6. ejecutar node:send(Msg). para hacer broadcast de un único mensaje
7. ejecutar node:generate(Mili, N) para hacer N broadcasts cada un tiempo aleatorio (el tiempo empieza con Mili(milisegundos) para poder poner un tiempo que permita ejecutar el comando en todas las terminales deseadas)

# Problemas prototipo 1:
- [ ] El resultado final mediante send queda en una lista (linea 78)
- [ ] El resultado final mediante generate queda en una tupla ({})
- [ ] Consultar sobre 2 mensajes al mismo tiempo que obtienen el mismo UniversalStamp
- [ ] Consultar sobre perdida de mensajes pero no de atomicidad y orden total (1)

- Problema actual: durante la negociacion de la UniversalStamp de un mensaje puede suceder que
se decida una que ya esté ocupada, en dicho caso esta se agrega al diccionario pisando la anterior.
- Posibles soluciones: 
    1. Ignorarlo: depende de la respuesta de la consulta (1).
    2. Implementar jerarquía de nodos y diccionario de listas (requiere refactor importante y es medio parche).
    3. Buscar alguna solución algoritmica (solución en la negociación de las UStamps, pedir una nueva UStamp si la actual está ocupada, etc).

- Observaciones: el ratio de perdida parece disminuir al aumentar la relacion entre nodos totales y nodos hablando. El programa parece funcionar bien dejando de lado los mensajes perdidos.

# Pendiente prototipo 1:
- Generar protocolo de cierre seguro
- Generar control de errores y caídas de nodos
- Realizar mas testing
- Solucionar problemas anteriores
- Consultar cómo usar el servicio como una "entidad unica"
- Consultar cómo usar la salida del servicio para el Ledger

# Make y testing
- Makefile
- Modulo make de Erlang (https://erlang.org/doc/man/make.html)
- Problema actual: no es posible ejecutar una funcion en un nodo remoto, esto hace que se deba hacer node:start(). en cada terminal.