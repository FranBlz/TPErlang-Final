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
9. ACLARACION: la funcion make results es horrible, preguntame si la queres usar

## Make y testing
- Makefile
- Modulo make de Erlang (https://erlang.org/doc/man/make.html)
- <mark>Problema actual:</mark> no es posible ejecutar una funcion en un nodo remoto, esto hace que se deba hacer node:start(). en cada terminal.

## TODO
 - Agregar la deteccion de errores cuando los hilos fallan
 - Informe
 - Testing
 - Implementar protocolo de inicio automático (opcional)

## Debugging
Resultados de una prueba en 30 nodos, 12 hablantes, 10 mensajes c/u, total recibidos 120:
> - Deliver: 45 node2@FranPC 7320 
> - Deliver: 47 node9@FranPC 3570 
> - Deliver: 47 node4@FranPC 751 
> - Deliver: 49 node23@FranPC 3704 

> - Deliver: 88 node17@FranPC 1368 
> - Deliver: 90 node9@FranPC 5660 
> - Deliver: 89 node4@FranPC 9562 
> - Deliver: 91 node2@FranPC 6948

Notar que en ambos casos el ganador de la propuesta es el nodo9 seguido del nodo4, no se presentan errores en el resto de victorias de dichos nodos. Notar también que en el resto de delivers desde el deliver 47 no vuelven a estar uno seguido del otro hasta el deliver 90.
No creo que todo esto sea coincidencia, habría que revisar si esto es un problema inherente del ISIS o si hay algo mal en la implementación.
- Consultar a Martin sobre esto
- Recrear la prueba hasta obtener el mismo problema pero con la opcion de guardar los resultados en archivos, comparar los archivos externamente para verificar si esto altera de alguan forma el orden total

Resultados de una prueba en 20 nodos, 12 hablantes, 20 mensajes c/u, total recibidos 240:
> - (node13@FranPC)2> Deliver: 104 node6@FranPC 8186 
> - (node13@FranPC)2> Deliver: 106 node9@FranPC 3518 
> - (node13@FranPC)2> Deliver: 106 node6@FranPC 6257 
> - (node13@FranPC)2> Deliver: 107 node4@FranPC 6272

> - (node13@FranPC)2> Deliver: 173 node12@FranPC 2336 
> - (node13@FranPC)2> Deliver: 175 node9@FranPC 5036 
> - (node13@FranPC)2> Deliver: 175 node4@FranPC 4472 
> - (node13@FranPC)2> Deliver: 176 node6@FranPC 4782


## Consulta 17/06:
- Cómo conectarse al servicio como una "entidad unica"?
- La idea de los servidores del Ledger (código 8) es modificar el archivo del bcast atomico? o son archivos distintos?
- Cómo controlar fallos por crash de un nodo?
- Comentar situación de errores a Martin.


# Consulta 15/06
Genial! Nuestras dudas son:
1. Nuestro algoritmo difiere ligeramente del ISIS que figura en Wikipedia. El ISIS envía el mensaje junto con una estampa propuesta, el resto de nodos responden con una estampa y almacenan el mensaje de forma provisional, finalmente el nodo original envía la estampa universal (calculada a partir de las estampas que recibió de los otros nodos) y los otros nodos reacomodan la pila acordemente. En nuestro algoritmo el nodo que quiere hacer bcast envía solo una petición de estampa junto con su estampa propuesta, el resto de nodos le respoden con sus estampas propuestas, el nodo original calcula la estampa universal y la envía al resto junto con el mensaje.
Es esta diferencia permitida o deberíamos seguir los pasos del algoritmo original al pie de la letra?
2. Se menciona en el enunciado que los nodos pueden fallar pero no ser erroneos, también que consideramos al medio como fiable, por lo que los únicos casos de error a considerar serían error por crasheo de un nodo y error por omisión, no?
3. Debemos contemplar la resinserción de nodos una vez iniciada la comunicación?
4. Debemos incluir algún tipo de automatización para el inicio de los nodos o se inician todos a mano en sus respectivas terminales?

Martín Ceresa
1. No estoy seguro si entendí bien la diferencia, la diferencia concreta es que el mensaje no se envía en su implementación? No recuerdo bien que es lo que dice ni bien sobre que supuestos están trabajando, pero mientras se envíe un paquete proponiendo asignarle tal numero de secuencia a un identificador único está bien. No necesita ser necesariamente un mensaje. Porque al final del día el nodo que respondió a la petición original debería saber a que mensaje o a qué le tiene que actualizar el numero de secuencia en caso que sea necesario.
2. Sí. Concretamente se asume que no hay nodos byzantinos.
3. No necesariamente, si lo hacen tienen que documentarlo.
4. Lo mismo que 3.

Bolzan Francisco
Muchas gracias.
No, nuestra implementación funciona así:
- NodoX quiere hacer bcast del mensaje Msg
- NodoX envía una petición de propuesta al resto de nodos junto con su estampa propuesta
- El resto de nodos reciben dicha petición y mandan una estampa al NodoX
- NodoX recibe dichas estampas y cuando tiene todas selecciona la mas alta (UStamp)
- NodoX envía {Msg, UStamp} al resto de nodos
- En nuestra implementación no hay un identificador único del mensaje ya que NodoX lo envía finalmente junto con su estampa definitiva, y como NodoX trabaja los mensajes de a uno sabe a que mensaje asignar dicha estampa.

Martín Ceresa
Claro, asumen que el nodo envía de un mensaje por vez. Eso no es necesariamente así

Bolzan Francisco
O sea que los nodos deberían poder procesar el envío de multiples mensajes en simultaneo?
Pregunto porque como lo tenemos ahora los mensajes eventualmente serán procesados y enviados, por lo que no se pierden mensajes a pesar de que los nodos los procesan de a uno. Sería necesario entonces que los nodos puedan procesar múltiples mensajes en simultaneo?

Martín Ceresa
No es necesario, pero deberán documentarlo.
Era un poco a la respuesta de porque estaba implementado así en Wikipedia o etc...

## Trabajo 19/6
Falta:
 - Informe
 - Falta hacer un refactor, hay muchas cosas que se repiten
 - Verificar por que falla el refactor, tira siempre los nombres de los nodos mas altos
 - Hay un error al seleccion de proposers en ledgerNode, donde seleccionamos el maximo cuando se llega al total de nodos (en old version)
 - Posible revisar: sistema de conexion para los nodos
 - Agregar el comando de net_kernel:connect al protocolo de inicio
 - Verificar si se puede cambiar el group leader al momento de spawnear un proceso en otro nodo. Esto nos ayudaria a hacer un mejor protocolo de inicio.
 - Revisar el problema en seccion debugging (en peor caso añadir a informe)
 - Realizar testing extensivo con todo terminado

# Deprecated
## Algunos resultados de tests:
- 20 nodos total, 8 nodos hablantes, 20 msgs c/u, 19 perdidos (11,875%)
- 20 nodos total, 8 nodos hablantes, 20 msgs c/u, 18 perdidos (11,25%)
- 20 nodos total, 8 nodos hablantes, 20 msgs c/u, 16 perdidos (10%)
- 20 nodos total, 2 nodos hablantes, 30 msgs c/u, 5 perdidos (8,33%)
- 20 nodos total, 10 nodos hablantes, 3 msgs c/u, 3 perdidos (10%)
- 5 nodos total, 5 nodos hablantes, 30 msgs c/u, 25 perdidos (16,67%)

## Problemas prototipo 1:
- El resultado final mediante send queda en una lista (linea 78, confirmo que esto es cuestion de los diccionarios, en el resto de funciones el dato no esta en una lista, habría que ver si en el ejemplo de Ceresa también pasaba y no nos dimos cuenta)
- ~~El resultado final mediante generate queda en una tupla ({})~~
- Consultar sobre 2 mensajes al mismo tiempo que obtienen el mismo UniversalStamp
- Consultar sobre perdida de mensajes pero no de atomicidad y orden total (1)

- ~~Problema actual: durante la negociacion de la UniversalStamp de un mensaje puede suceder que
se decida una que ya esté ocupada, en dicho caso esta se agrega al diccionario pisando la anterior.~~
- Posibles soluciones: 
    1. Ignorar la perdida de mensajes: depende de la respuesta de la consulta (1).
    2. Imprimirlo sin esperar (sacado de bibliografía, habría que saber si en realidad esto es un problema recurrente del algoritmo o no, porque si lo es no estaría mal esta solución, pero si es un problema de nuestra implementación no me parece del todo correcta)
    3. Implementar jerarquía de nodos y diccionario de listas (requiere refactor importante y es medio parche).
    4. Implementar un reenvio de mensajes (donde está el PISADO actualmente reiniciar la votacion de UStamp).
    5. Implementar algun tipo de mutex para la votación (?)
    6. Descongestionar la red (?)
    7. Descongestionar los nodos internamente (?)
    8. Buscar alguna solución algoritmica.

- ~~Observaciones: el ratio de perdida parece disminuir al aumentar la relacion entre nodos totales y nodos hablando. El programa parece funcionar bien dejando de lado los mensajes perdidos. Sorprendentemente, el orden total no se ve afectado por la perdida e mensajes, no se por que será esto ya que a mi entender llega mas de 1 mensaje con la misma UStamp al diccionario, pero parece que el mensaje pisado siempre es el mismo a través de los nodos...habría que revisarlo.~~
- ~~<mark>IMPORTANTE:</mark> habría que consultar lo que hablamos la otra vez sobre estos mensajes que se enumeran igual, por lo que leí es algo que puede pasar (mirar fondo del README), pero valdría la pena consultarlo antes de implementar soluciones al problema.~~

## Consultas:
- ~~La conexión de los nodos debe ser parte del programa?~~
- ~~Que significa que un nodo pueda fallar pero no ser erroneo (crash y omisión?).~~
- ~~Hay que contemplar la reinserción o adición de nodos una vez iniciada la red?~~
- ~~Nuestra implementación difiere ligeramente del algoritmo ISIS (ISIS envia el mensaje, negocia la estampa, guarda el mensaje provisional, envia la estampa universal, actualiza el mensaje. Nuestro algoritmo envia peticion de negociar, negocia la estampa, envia el mensaje junto con la estampa final).~~

## Aclaración importante
En wikipedia dice que de llegar un mensaje con valor menor o igual al contador se imprime sin esperar:
> Una vez escogido el número de secuencia se manda a todos los procesos, se reordena el buzón y se procede a realizar el envío del mensaje.
> Si el acuerdo que llega a un proceso tiene un valor mayor a los guardados en el buzón, esperaremos a que lleguen los demás acuerdos para ir sacando del buzón y enviado el mensaje del menor al mayor número de secuencia.
> Si el acuerdo que llega a un proceso tiene un valor menor que los guardados en el buzón, podemos enviar directamente este mensaje sin necesidad de esperar.

En el paper que ellos dieron primero dice que esto no debería suceder (pag 18):
> Messages are delivered in the order of their global timestamp, that is, a message m can only be delivered once it has been assigned its global timestamp sn(m), and no other undelivered message m can possibly receive a timestamp sn(m) smaller or equal to sn(m)

Sin embargo inmediatamente despues menciona que:
> As with the communication history algorithm (Figure 11), the identifier of the message sender is used to break ties between messages with the same global timestamp.

## Observaciones
ISIS mantiene una ventaja sobre nuestra implementación que es la posibilidad de que un nodo procese el envío de múltiples mensajes a la vez. Esto no afecta a las propiedades de liveness, correctitud y atomicidad, pero aún así es algo a tener en cuenta. Podemos seguir adelante como estamos y documentarlo ó cambiar el código para seguir la implementación original (requiere un refactor importante).