:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(0, []).

ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

ej(4, [rombo]).

ej(5, [rombo, espacio, cuadrado, espacio, sol, luna]).

esp(espacio). % no estoy seguro de como usar espacio si no es de esta forma


%%%%%%%%

% 2- juntar_con(L, J, R)
juntar_con([X], J, X).
juntar_con([X, Y | Ltail], J, R) :- append(X, [J | LtailRec], R), juntar_con([Y | Ltail], J, LtailRec).
juntar_con([], _, []).

% 3- palabras(S, P)
tieneEspacio(P) :- member(L, P), member(espacio, L).
palabras(S, P) :- juntar_con(P, espacio, S), not(tieneEspacio(P)).

% 4- asignar var(A, MI, MF)
equal([X | LS], [X| LS2]) :- equal(LS, LS2).
equal([], []).

asignar_var(A, [], [(A, _)]).
asignar_var(A, [(B, C) | LS], [(B, C) | LS2]) :- asignar_var(A, LS, LS2), A \= B.
asignar_var(A, [(A, C) | LS], [(A, C) | LS2]) :- equal(LS,LS2). %TODO - buscar el operador para equal
% TODO - fijarse si el orden importa, o si tiene que tirar todas las opciones
% asignar_var(rombo, [(cuadrado, _G4013),(rombo, _G4012)], M).
% M = [(cuadrado, _G4013),(rombo, _G4012)],

% 5- palabras con variables(P, V)
palabras_con_variables(XSS, YSS) :- palabras_aux(XSS, YSS, []).

palabras_aux([], [], _) :- true, !. % TODO - preguntar como hacer un cut aca
palabras_aux([[] | XSS], [[] | YSS], M) :- palabras_aux(XSS, YSS, M).
palabras_aux([[X | XS] | XSS], [[Y | YS] | YSS], MI) :- asignar_var((X, Y), MI, MF), palabras_aux([XS | XSS], [YS | YSS], MF).


% 6- quitar(E, L, R)
quitar(A, [B | XS], YS) :- quitar(A, XS, YS), A == B.
quitar(A, [B | XS], [B | YS]) :- quitar(A, XS, YS), A \= B.
quitar(_, [], []).
