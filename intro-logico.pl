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

