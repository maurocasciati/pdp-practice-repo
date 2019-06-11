tarea(administrativo, 6).
tarea(dormirEnElCongreso, 1).
tarea(prensa, 7).
tarea(canillita, 8).

trabajo(claudia, administrativo).
trabajo(jorge, dormirEnElCongreso).
trabajo(alberto, dormirEnElCongreso).
trabajo(mariano, canillita).
trabajo(mariano, prensa).

poder(jorge, ejecutivo).
poder(alberto, legislativo).
poder(anibal, legislativo).
poder(marisa, prensa).
poder(pedro, judicial).
poder(pedro, legislativo).

partido(jorge, parasitosPoliticos).
partido(alberto, parasitosPoliticos).
partido(anibal, parasitosPoliticos2).

%% Conocer todas las tareas estresantes. Se considera una tarea estresante si su complejidad mayor a 5.
tareaEstresante(Tarea):- 
    tarea(Tarea,Complejidad), 
    Complejidad > 5.

%% Se quiere conocer a todas las personas que están estresadas. Se considera a una persona estresada 
%% si realiza al menos una tarea estresante o si pertenece al poder ejecutivo.
personaEstresada(Trabajador):- 
    trabajo(Trabajador, Tarea), 
    tareaEstresante(Tarea).
personaEstresada(Trabajador):- 
    poder(Trabajador, ejecutivo).

%% Se quiere conocer si un trabajador se encuentra en peligro, teniendo en cuenta que todas las 
%% personas estresadas que no pertenecen al poder legislativo se encuentran en peligro como así 
%% tambien todas las que trabajan en la prensa.
trabajadorEnPeligro(Trabajador):- 
    personaEstresada(Trabajador), 
    not(poder(Trabajador,legislativo)).
trabajadorEnPeligro(Trabajador):- 
    poder(Trabajador, prensa).

%% Los trabajadores tienen enemigos natos. Estos están dados por los poderes en los que trabaja cada uno. 
%% Desarrollar una solución teniendo en cuenta que : 
%%         Se desea todas las personas enemigas entre si.
%%     Los trabajadores del poder judicial son enemigos de los del poder ejecutivo.  

sonEnemigos(Trabajador1, Trabajador2):-
    enemigoNato(Trabajador1, Trabajador2).
sonEnemigos(Trabajador1, Trabajador2):-
    enemigoNato(Trabajador2, Trabajador1).

enemigoNato(Trabajador1, Trabajador2):- 
    poder(Trabajador1, judicial), 
    poder(Trabajador2, ejecutivo).

%%     Los trabajadores del poder legislativo
%%         son enemigos de los del poder judicial.
%%         son enemigos de los legisladores de otros partidos
enemigoNato(Trabajador1, Trabajador2):- 
    poder(Trabajador1, legislativo), 
    poder(Trabajador2, judicial).
enemigoNato(Trabajador1, Trabajador2):- 
    poder(Trabajador1, legislativo), 
    poder(Trabajador2, legislativo),
    partido(Trabajador1, Partido1), 
    partido(Trabajador2, Partido2), 
    Partido1 \= Partido2.

%     Los trabajadores de la prensa son enemigos de todos (menos de si mismos).
enemigoNato(Trabajador1, Trabajador2):- 
    poder(Trabajador1, prensa), 
    Trabajador1 \= Trabajador2.

% Encontrar el trabajador que tiene la tarea con mayor complejidad.
trabajadorTareaMayorComplejidad(Trabajador):- 
    trabajo(Trabajador,Tarea),
    tarea(Tarea,Complejidad),
    not((tarea(_,ComplejidadX),Complejidad < ComplejidadX)). % not es el inverso de forall (no existe otra tarea cuya complejidad menor a mi complejidad)

% trabajadorTareaMayorComplejidad(Trabajador):-
%     trabajo(Trabajador,Tarea),
%     tarea(Tarea,Complejidad),
%     forall(tarea(_,ComplejidadX), Complejidad >= ComplejidadX). 

% Encontrar a todos los trabajadores que tienen una única tarea.
trabajadorConUnicaTarea(Trabajador):-
    trabajo(Trabajador, Tarea),
    forall(trabajo(Trabajador, UnaTarea), Tarea = UnaTarea).

%trabajadorConUnicaTarea(Trabajador):-
%    trabajo(Trabajador, Tarea),
%    not((trabajo(Trabajador, UnaTarea), Tarea \= UnaTarea)).


%       Punto extra:
% proyecto(nombreProyecto, diaDeInicio, poder)
% precedente(tarea, tareaPrevia, proyecto)
% Teniendo en cuenta que una tarea dura la misma cantidad de días que su complejidad, ¿Que día termina el proyecto?

%7.Los proyectos estan relacionados con cada poder. Los dias que lleva cada tarea estan relacionados con la complejidad. 
%Las tareas se concatenan! Una tarea no puede comenzarse si no se completo la anterior. 
%proyecto(PROYECTO, DIA DE INICIO, PODER). 
%procedencia(TAREA, TAREAPREVIA, PROYECTO). 
%¿que dia termina el proyecto? COMPLEJIDAD = DIAS QUE LLEVA.


proyecto(leyDePymes, 3, legislativo). 
proyecto(anunciosEconomicos, 4, ejecutivo). 
proyecto(juicioFinal, 1, judicial).  
proyecto(guerraDePrensa, 1, prensa). 

tarea(votarLeyes, 4).
tarea(redactarLeyes, 8).
tarea(investigar, 6).
tarea(redactarInformes, 5).
tarea(defender, 8).
tarea(juzgar, 8).
tarea(cenarConOtrosLideres, 5).
tarea(tareasAdministrativas, 6).
tarea(presidirPais, 5).
tarea(investigarOposicion, 5).

procedencia(votarLeyes, redactarLeyes, leyDePymes). 
procedencia(investigar, votarLeyes, leyDePymes). 
procedencia(redactarInformes, investigar, leyDePymes). 
procedencia(defender, investigar, juicioFinal). 
procedencia(juzgar, defender, juicioFinal). 
procedencia(cenarConOtrosLideres, tareasAdministrativas, anunciosEconomicos). 
procedencia(presidirPais, cenarConOtrosLideres, anunciosEconomicos). 
procedencia(investigar, investigarOposicion, guerraDePrensa). 

esTareaInicial(Tarea, Proyecto):- 
    procedencia(TareaPadre, Tarea, Proyecto), 
    not(procedencia(Tarea, _, Proyecto)).

diasDeProyecto(Tarea, Proyecto, Dias):-
    esTareaInicial(Tarea, Proyecto),
    tarea(Tarea, DuracionTarea),
    proyecto(Proyecto, DiaInicio, _), 
    Dias is DiaInicio + DuracionTarea. 
    
diasDeProyecto(Tarea, Proyecto, DiasTotales):- 
    tarea(Tarea, Dias), 
    procedencia(Tarea, TareaPadre, Proyecto), 
    diasDeProyecto(TareaPadre, Proyecto, DiasPasados), 
    DiasTotales is Dias + DiasPasados. 
    
diaQueTerminaElProyecto(Proyecto,DiasTotales):- 
    esTareaFinal(Tarea,Proyecto), 
    diasDeProyecto(Tarea, Proyecto, DiasTotales). 
    
esTareaFinal(Tarea,Proyecto):- 
    procedencia(Tarea, TareaPrevia,Proyecto), 
    not(procedencia(_, Tarea, Proyecto)).