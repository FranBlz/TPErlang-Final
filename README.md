# TPErlang-Final

## Protocolo de inicio:
1. Asegurarse que no haya ningun .beam
2. make o make results (si se quiere guardar los resultados de las terminales en archivos)
3. en una terminal de las generadas ejecutar make:all().
4. en la misma terminal ejecutar conectar:con(X). donde X es el número de terminales, debe coincidir con NODOS en makefile
5. ejecutar node:start(). en todas las terminales
6. ejecutar node:send(Msg). para hacer broadcast de un único mensaje
7. ejecutar node:generate(Mili, N) para hacer N broadcasts cada un tiempo aleatorio (el tiempo empieza con Mili(milisegundos) para poder poner un tiempo que permita ejecutar el comando en todas las terminales deseadas)
8. si se usó make results, cerrar las terminales una a una con q().

## Problemas prototipo 1:
- El resultado final mediante send queda en una lista (linea 78, confirmo que esto es cuestion de los diccionarios, en el resto de funciones el dato no esta en una lista, habría que ver si en el ejemplo de Ceresa también pasaba y no nos dimos cuenta)
- ~~El resultado final mediante generate queda en una tupla ({})~~
- Consultar sobre 2 mensajes al mismo tiempo que obtienen el mismo UniversalStamp
- Consultar sobre perdida de mensajes pero no de atomicidad y orden total (1)

- Problema actual: durante la negociacion de la UniversalStamp de un mensaje puede suceder que
se decida una que ya esté ocupada, en dicho caso esta se agrega al diccionario pisando la anterior.
- Posibles soluciones: 
    1. Ignorar la perdida de mensajes: depende de la respuesta de la consulta (1).
    2. Imprimirlo sin esperar (sacado de bibliografía, habría que saber si en realidad esto es un problema recurrente del algoritmo o no, porque si lo es no estaría mal esta solución, pero si es un problema de nuestra implementación no me parece del todo correcta)
    3. Implementar jerarquía de nodos y diccionario de listas (requiere refactor importante y es medio parche).
    4. Implementar un reenvio de mensajes (donde está el PISADO actualmente reiniciar la votacion de UStamp).
    5. Implementar algun tipo de mutex para la votación (?)
    6. Descongestionar la red (?)
    7. Descongestionar los nodos internamente (?)
    8. Buscar alguna solución algoritmica.

- Observaciones: el ratio de perdida parece disminuir al aumentar la relacion entre nodos totales y nodos hablando. El programa parece funcionar bien dejando de lado los mensajes perdidos. Sorprendentemente, el orden total no se ve afectado por la perdida e mensajes, no se por que será esto ya que a mi entender llega mas de 1 mensaje con la misma UStamp al diccionario, pero parece que el mensaje pisado siempre es el mismo a través de los nodos...habría que revisarlo.
- <mark>IMPORTANTE:</mark> habría que consultar lo que hablamos la otra vez sobre estos mensajes que se enumeran igual, por lo que leí es algo que puede pasar (mirar fondo del README), pero valdría la pena consultarlo antes de implementar soluciones al problema.

## Algunos resultados de tests:
- 20 nodos total, 8 nodos hablantes, 20 msgs c/u, 19 perdidos (11,875%)
- 20 nodos total, 8 nodos hablantes, 20 msgs c/u, 18 perdidos (11,25%)
- 20 nodos total, 2 nodos hablantes, 30 msgs c/u, 5 perdidos (8,33%)
- 20 nodos total, 10 nodos hablantes, 3 msgs c/u, 3 perdidos (10%)

## Pendiente prototipo 1:
- Generar protocolo de cierre seguro
- Generar control de errores y caídas de nodos
- Realizar mas testing
- Solucionar problemas anteriores
- Consultar cómo usar el servicio como una "entidad unica"
- Consultar cómo usar la salida del servicio para el Ledger

## Make y testing
- Makefile
- Modulo make de Erlang (https://erlang.org/doc/man/make.html)
- <mark>Problema actual:</mark> no es posible ejecutar una funcion en un nodo remoto, esto hace que se deba hacer node:start(). en cada terminal.

## Aclaración importante
En wikipedia dice que de llegar un mensaje con valor menor o igual al contador se imprime sin esperar:
> Una vez escogido el número de secuencia se manda a todos los procesos, se reordena el buzón y se procede a realizar el envío del mensaje.
> Si el acuerdo que llega a un proceso tiene un valor mayor a los guardados en el buzón, esperaremos a que lleguen los demás acuerdos para ir sacando del buzón y enviado el mensaje del menor al mayor número de secuencia.
> Si el acuerdo que llega a un proceso tiene un valor menor que los guardados en el buzón, podemos enviar directamente este mensaje sin necesidad de esperar.

En el paper que ellos dieron primero dice que esto no debería suceder (pag 18):
> Messages are delivered in the order of their global timestamp, that is, a message m can only be delivered once it has been assigned its global timestamp sn(m), and no other undelivered message m can possibly receive a timestamp sn(m) smaller or equal to sn(m)

Sin embargo inmediatamente despues menciona que:
> As with the communication history algorithm (Figure 11), the identifier of the message sender is used to break ties between messages with the same global timestamp.