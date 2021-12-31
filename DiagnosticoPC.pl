/*
INTERFAZ GRAFICA: Esta parte del sistema es la que se encarga de
interactuar el usuario, mostrar imagenes, botones, textos, etc.
*/
 :- use_module(library(pce)).
 :- pce_image_directory('./imagenes').
 :- use_module(library(pce_style_item)).
 :- dynamic color/2.
%se cargan la imagen principal donde se mostraran la solucion
 resource(img_principal, image, image('img_principal.jpg')).
%se carga la imagen de la portada
 resource(portada, image, image('portada.jpg')).

%se cargan las imagenes para mostrarlas en la ventana de soluciones 
 resource(falla_memoria_ram, image, image('falla_memoria_ram.jpg')).
 resource(falla_disco_duro, image, image('falla_disco_duro.jpg')).
 resource(falla_tarjeta_video, image, image('falla_tarjeta_video.jpg')).
 resource(falla_virus, image, image('falla_virus.jpg')).
 resource(desconocido, image, image('desconocido.jpg')).

%se cargan las imagenes de todas las preguntas
 resource(pantalla_azul, image, image('pantalla_azul.jpg')).
 resource(instalar_programa, image, image('instalar_programa.jpg')).
 resource(error_aranque, image, image('error_aranque.jpg')).
 resource(detiene_aleatorio, image, image('detiene_aleatorio.jpg')).

 resource(ruido, image, image('ruido.jpg')).
 resource(lenta, image, image('lenta.jpg')).
 resource(negar_acceso, image, image('negar_acceso.jpg')).
 resource(error_guardar, image, image('error_guardar.jpg')).

 resource(apaga, image, image('apaga.jpg')).
 resource(no_video, image, image('no_video.jpg')).
 resource(caliente, image, image('caliente.jpg')).
 resource(rayas_pantalla, image, image('rayas_pantalla.jpg')).

 resource(lenta_apaga, image, image('lenta_apaga.jpg')).
 resource(duplica_archivos, image, image('duplica_archivos.jpg')).
 resource(abren_ventanas, image, image('abren_ventanas.jpg')).
 resource(eliminan, image, image('eliminan.jpg')).

 mostrar_imagen(Pantalla, Imagen) :- new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(0,0)).
  mostrar_imagen_tratamiento(Pantalla, Imagen) :-new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(60,180)).
 nueva_imagen(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(0,0)).
  imagen_pregunta(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(500,60)).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
  botones:-borrado,
                send(@boton, free),
                send(@btntratamiento,free),
                mostrar_diagnostico(Enfermedad),
                send(@texto, selection('El Diagnostico a partir de los datos es:')),
                send(@resp1, selection(Enfermedad)),
                new(@boton, button('Iniciar consulta',
                message(@prolog, botones)
                )),

                new(@btntratamiento,button('Detalles y Tratamiento',
                message(@prolog, mostrar_tratamiento,Enfermedad)
                )),
                send(@main, display,@boton,point(20,450)),
                send(@main, display,@btntratamiento,point(138,450)).



  mostrar_tratamiento(X):-new(@tratam, dialog('Tratamiento')),
                          %send(@tratam, append, label(nombre, 'Explicacion: ')),
                          send(@tratam, display,@lblExp1,point(70,5)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          send(@tratam, display,@r1,point(15,25)),
                          send(@tratam, display,@r2,point(15,40)),
                          send(@tratam, display,@r3,point(15,55)),
                          send(@tratam, display,@r4,point(15,70)),
                          send(@tratam, display,@r5,point(15,85)),
                          send(@tratam, display,@r6,point(15,100)),
                          send(@tratam, display,@r7,point(15,115)),
                          send(@tratam, display,@r8,point(15,130)),
                          send(@tratam, display,@r9,point(15,145)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

tratamiento(X):- send(@lblExp1,selection('Soluciones')),
                 mostrar_imagen_tratamiento(@tratam,X),
                 tratamientoE(X).

tratamientoE(X):-X == 'falla_memoria_ram', solucion_memoria_ram; X == 'falla_disco_duro', solucion_falla_disco_duro; 
X == 'falla_tarjeta_video', solucion_falla_tarjeta_video; X == 'falla_virus', solucion_falla_virus.

solucion_memoria_ram:-
send(@r1,selection('Posibles soluciones para falla en la memoria RAM:')),
send(@r2, selection('Cambia la memoria a otra ranura o si tienes dos, cambialas entre ellas.')),
send(@r3, selection('Tambien puedes probarlas de forma independiente y asi descartar que alguna de ellas este estropeada.')),
send(@r4, selection('Si has cambiado o actualizado los modulos de memoria intenta volver a su estado anterior antes de haber insertado la memoria RAM.')),
send(@r5, selection('Si  funciona, es muy probable que  tengas un problema de compatibilidad o que la memoria este mal.')),
send(@r6, selection('Si sigues teniendo problemas, la causa puede venir de otro sitio.')).

solucion_falla_disco_duro:-
send(@r1,selection('Posibles soluciones para fallas en el disco duro:')),
send(@r2,selection('Si se escucha un ruido')),
send(@r3,selection('se refiere a que los cabezales del lector escritura, han aterrizado sobre lasuperficie de los platos')),
send(@r4,selection('para este caso no hay solucion, solo reemplazarlo por otro disco duro.')),
send(@r5,selection('En el caso de que no guarde informacion')),
send(@r6,selection('realizar un diagnostico general del disco duro con algun software parareparar sectores danados (Software RecomendadoHiren´s Boot CD).')),
send(@r7,selection('si la computadora esta lenta')),
send(@r8,selection('desfragmenta tu disco con las herramientas de windows')).

solucion_falla_tarjeta_video:-
send(@r1,selection('Posibles soluciones para fallas en la tarjeta de video')),
send(@r2,selection('Cambiar los drivers')),
send(@r3,selection('limpia tu computadora cada 6 meses')),
send(@r4,selection('revisa si esta bien conectada')),
send(@r5,selection('verifica el monitos')),
send(@r6,selection('cambia de tarjeta grafica')).

solucion_falla_virus:-
send(@r1,selection('Posibles soluciones para fallas ocasionadas por virus:')),
send(@r2,selection('instala un ativirus')),
send(@r3,selection('manten tu equipo actualizado')),
send(@r4,selection('instala un atimalware ')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%se muestra la interfaz de las preguntas y se espera una respuesta que luego se pasa a las inferencias
   preguntar(Preg,Resp):-new(Di,dialog('Colsultar Datos:')),
                        new(L2,label(texto,'Responde las siguientes preguntas')),
                        id_imagen_preg(Preg,Imagen),
                        imagen_pregunta(Di,Imagen),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  interfaz_principal:-new(@main,dialog('Diagnostico PC',size(1000,1000))),
        new(@texto, label(nombre,'El Diagnostico a partir de los datos es:',font('times','roman',18))),
        new(@resp1, label(nombre,'',font('times','roman',24))),
        new(@lblExp1, label(nombre,'',font('times','roman',14))),
        new(@lblExp2, label(nombre,'',font('times','roman',14))),

        new(@r1, label(nombre,'',font('times','roman',16))),
        new(@r2, label(nombre,'',font('times','roman',14))),
        new(@r3, label(nombre,'',font('times','roman',14))),
        new(@r4, label(nombre,'',font('times','roman',14))),
        new(@r5, label(nombre,'',font('times','roman',14))),
        new(@r6, label(nombre,'',font('times','roman',14))),
        new(@r7, label(nombre,'',font('times','roman',14))),
        new(@r8, label(nombre,'',font('times','roman',14))),
        new(@r9, label(nombre,'',font('times','roman',14))),
        new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
        new(@boton, button('Iniciar consulta',message(@prolog, botones))),

        new(@btntratamiento,button('¿Tratamiento?')),

        nueva_imagen(@main, img_principal),
        send(@main, display,@boton,point(138,450)),
        send(@main, display,@texto,point(60,100)),
        send(@main, display,@salir,point(300,450)),
        send(@main, display,@resp1,point(100,150)),
        send(@main,open_centered).

       borrado:- send(@resp1, selection('')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interfaz:- new(@interfaz,dialog('Sistema para diagnosticar pc',
  size(590,590))),
  new(@titulo, label(nombre,'    BIENVENIDO A DIAGNOSTICA TU PC',font(times, bold, 30))),send(@titulo,colour,purple),
  
  new(@titulo2, label(nombre,'   Instituto Tecnologico De Tijuana',font(times, bold, 20))),send(@titulo2,colour,blue),
  new(@info, label(nombre,'                                                Ingenieria En Sistemas Computacionales',font(scren, italic, 15))),send(@info,colour,blue),
  new(@info2, label(nombre,'                                                    Programacion Logica Y Funcional',font(scren, italic, 15))),send(@info2,colour,blue),
  new(@info3, label(nombre,'                                                         Alumno y No. Control',font(scren, italic, 15))),send(@info3,colour,blue),
  new(@info4, label(nombre,'                                                    Omar Reyes Zamudio - 18212257',font(scren, italic, 15))),send(@info4,colour,blue),


  new(BotonComenzar,button('COMENZAR',and(message(@prolog,interfaz_principal) ,
  and(message(@interfaz,destroy),message(@interfaz,free)) ))),
  new(BotonSalir,button('SALIDA',and(message(@interfaz,destroy),message(@interfaz,free)))),

   mostrar_imagen(@interfaz, portada),

  send(@interfaz, display,BotonComenzar,point(300,550)),
  send(@interfaz, display,BotonSalir,point(400,550)),

  send(@interfaz, display,@titulo,point(10,20)),
  send(@interfaz, display,@titulo2,point(200,60)),
  send(@interfaz, display,@info,point(20,400)),
  send(@interfaz, display,@info2,point(20,420)),
  send(@interfaz, display,@info3,point(20,440)),
  send(@interfaz, display,@info4,point(20,460)),
  send(@interfaz,open_centered).

  :-interfaz.

/* BASE DE CONOCIMIENTOS: fallas y sintomas de la pc contienen ademas
el identificador de imagenes de acuerdo al  sintoma
*/

conocimiento('falla_memoria_ram',
['la computadora presenta pantallazos azules aleatorios?', 'windows falla en la instalacion de programas?',
'windows falla en el arranque?','la computadora se detiene de manera aleatoria?']).

conocimiento('falla_disco_duro',
['escucha un ruido de clic o rascar?', 'la computadora se pone lenta?',
'le niega el acceso a ver las particiones?','muestra errores al guardar la informacion?']).

conocimiento('falla_tarjeta_video',
['la computadora se apaga al poco tiempo de encendido?','la fuente de poder enciende pero no hay señal de video?', 
'la computadora se calienta demasiado?', 'aparecen rayas verticales o rayas horizontales de manera aleatoria o fija?']).

conocimiento('falla_virus',
['es muy lenta en el encendido y/o apagado?', 'se duplican algunos de los archivos?',
 'se abren ventanas solas?','se eliminan y modifican archivos?']).


id_imagen_preg('la computadora presenta pantallazos azules aleatorios?','pantalla_azul').
id_imagen_preg('windows falla en la instalacion de programas?','instalar_programa').
id_imagen_preg('windows falla en el arranque?','error_aranque').
id_imagen_preg('la computadora se detiene de manera aleatoria?','detiene_aleatorio').

id_imagen_preg('escucha un ruido de clic o rascar?','ruido').
id_imagen_preg('la computadora se pone lenta?','lenta').
id_imagen_preg('le niega el acceso a ver las particiones?','negar_acceso').
id_imagen_preg('muestra errores al guardar la informacion?','error_guardar').

id_imagen_preg('la computadora se apaga al poco tiempo de encendido?','apaga').
id_imagen_preg('la fuente de poder enciende pero no hay señal de video?','no_video').
id_imagen_preg('la computadora se calienta demasiado?','caliente').
id_imagen_preg('aparecen rayas verticales o rayas horizontales de manera aleatoria o fija?','rayas_pantalla').

id_imagen_preg('es muy lenta en el encendido y/o apagado?','lenta_apaga').
id_imagen_preg('se duplican algunos de los archivos?','duplica_archivos').
id_imagen_preg('se abren ventanas solas?','abren_ventanas').
id_imagen_preg('se eliminan y modifican archivos?','eliminan').

 /* INFERENCIA: Esta parte del sistema que se encarga de
 inferir cual es el diagnostico a partir de las preguntas realizadas
 */
:- dynamic conocido/1.
  % se limpia la ventana si se hace un dignostico y muestra el nombre del fallo que encontro al hacer el diagnostico
  mostrar_diagnostico(X):-haz_diagnostico(X),limpiar_ventana.
  %si el dignostico no existe en tonces se muetra que es desconocido
  mostrar_diagnostico(desconocido):-limpiar_ventana.
 %se obtiene el diagnostico si existen una bese de conocimiento y se valida la prueba de presencia
  haz_diagnostico(Diagnosis):-

                            obten_hipotesis_y_sintomas(Diagnosis,
                            ListaDeSintomas),
                            prueba_presencia_de(Diagnosis,
                            ListaDeSintomas).

%se obtiene una hipotesis y un sintoma si se conoce el fallo y sus sintoma
obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas):-
                           conocimiento(Diagnosis, ListaDeSintomas).

%dependiendo de determinado fallo se hace el cuestionario para ver si cumple con toda la lsita de nuesta base de conocimiento
prueba_presencia_de(Diagnosis, []).
%la prueba de presencia es verdadera si cuando se hace la prueba de verdad se acepta esto se repite hasta que se acabe la lista
prueba_presencia_de(Diagnosis, [Cabeza | Cola]):-
  prueba_verdad_de(Diagnosis, Cabeza), prueba_presencia_de(Diagnosis, Cola).

%si se responde con si entonces existe la prueba de verdad
prueba_verdad_de(Diagnosis, Sintoma):- conocido(Sintoma).
%si se responde con algo que no sea si entonces se cambia de pregunta
prueba_verdad_de(Diagnosis, Sintoma):-
 not(conocido(is_false(Sintoma))),
 pregunta_sobre(Diagnosis, Sintoma, Resp), Resp = 'si'.

%se hace una pregunta de un sintoma si se respondio la pregunta
 pregunta_sobre(Diagnosis, Sintoma, Resp):-
  %se hace una pregunta y obtiene la respuesta de si o no
 preguntar(Sintoma,Respuesta),
   process(Diagnosis, Sintoma, Respuesta, Resp).

%al responer que si sabemos que nuestro equipo presenta ese sintoma
process(Diagnosis, Sintoma, si, si):- asserta(conocido(Sintoma)).
%al responder que no inferimos que el sintoma no es un problema que presente nuestra computadora
process(Diagnosis, Sintoma, no, no):- asserta(conocido(is_false(Sintoma))).


limpiar_ventana:- retract(conocido(X)), fail.
limpiar_ventana.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).

















