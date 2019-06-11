% VS PLUGIN: VSC-Prolog
% ------- Ejemplos de la primera clase 

% Set de datos. Si no esta aca, no va a dar true 
esXadre(ale,mau).
esXadre(gabi,mau).
esXadre(ale,agus).
esXadre(gabi,agus).

% Funciones: 
esHije(Hije,Xadre):-
  esXadre(Xadre,Hije).
  
% Esto me devuelve el set de datos donde se cumple: (mau y agus) 
esXadre(ale,X).
  
% Recursividad: 
esAncestro(Ancestro,Descendiente):-
  esXadre(Ancestro,Descendiente).
esAncestro(Ancestro,Descendiente):-
  esXadre(Ancestro,X):-
  esAncestro(X,Descendiente).

% -------------- 2da clase ------------------------ 

% %
esPersona(ana).
esPersona(juan).
esRico(ana).

% esPobre(Persona):- not(esRico(Persona)).
% esto devuelve true para cualquier cosa que le pongas
% que no sea ana, ya que no se fija si es o no perona.
% Entonces lo cambiamos por:
esPobre(Persona):- esPersona(Persona), not(esRico(Persona)).


% Calculos con IS (en vez de =)
precio(cafe,25).
precio(auriculares,250).
precio(mouse,400).

dinero(viotti,200).
dinero(devoto,1030).

compra(Persona,Producto):-
  dinero(Persona,Dinero),
  precio(Producto,Precio),
  Dinero >= Precio,
  Cantidad is Dinero / Precio.

% Inversivilidad: Conviene usarlo porque permite usar las cosas de distinta manera. buscar

% ------------ Listas:
% LENGTH
% -/ length(1,2,3,4,5) %devuelve 5
% -/ length(X,5) %devuelve listas con 5 elementos que son cualquier cosa

% Teoria: Los predicados estan cuantificados existencialmente (o sea, existe tal cosa, lo que no esta en predicados no)
% FORALL
profesion(juan,herrero).
profesion(ana,carpintera).
profesion(pedro,herrero).

tieneCuchillo(juan,palo).
tieneCuchillo(ana,metal).
tieneCuchillo(pedro,metal).

enCasaDeHerreroCuchilloDePalo(Fulano?):-
  profesion(Fulano, herrero),
  tieneCuchillo(Fulano, palo).
  
% enCasaDeHerreroCuchilloDePalo(X) me devuelve juan
% enCasaDeHerreroCuchilloDePalo(_) me devuelve true porque existe aunque sea 1

enCasaDeHerreroCuchilloDePaloSiempre:-
  forall(profesion(Fulano, herrero),
    tieneCuchillo(Fulano, palo)). %devuelve true si siempre se cumple    

% FORALL CON MAS DE DOS PARAMETROS
alQueMadrugaDiosLoAyuda:-
  forall(madruga(Fulano),
        (loAyuda(Dios,Fulano),esDios(Dios))).

madruga(juan).
madruga(ana).
loAyuda(juan,yaleh).
loAyuda(ana,diablo).
esDios(ala).
esDios(yaleh).
    
% Ejercicio: hacer un nuevo refran:




