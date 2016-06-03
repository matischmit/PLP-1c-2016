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

% IMPORTANTE: No se permite usar findall, setof ni cuts. Sí se permite not.

%1 - diccionario_lista(?L)
diccionario_lista(X) :- diccionario(S), string_codes(S,X).

% 2- juntar_con(?L, ?J, ?R)
juntar_con([X], _, X).
juntar_con([X, Y | Ltail], J, R) :- append(X, [J | LtailRec], R), juntar_con([Y | Ltail], J, LtailRec).
juntar_con([], _, []).

% 3- palabras(?S, ?P)
% OBS: No pueden estar ambas sin instanciar
tieneEspacio(P) :- member(L, P), member(espacio, L).
palabras(S, P) :- juntar_con(P, espacio, S), not(tieneEspacio(P)).

% 4- asignar var(A, MI, MF)
% OBS: Preguntamos y el orden NO importa, y sólo tiene que devolver una opción
equal(XS,XS).

asignar_var(A, [], [(A, _)]).
asignar_var(A, [(B, C) | LS], [(B, C) | LS2]) :- asignar_var(A, LS, LS2), A \= B.
asignar_var(A, [(A, C) | LS], [(A, C) | LS2]) :- equal(LS,LS2).%LS=LS2. %TODO - PREGUNTAR buscar el operador para equal


% 5- palabras con variables(P, V)
palabras_con_variables(XSS, YSS) :- palabras_aux(XSS, YSS, []).

palabras_aux([], [], _).
palabras_aux([[] | XSS], [[] | YSS], M) :- palabras_aux(XSS, YSS, M).
palabras_aux([[X | XS] | XSS], [[Y | YS] | YSS], MI) :- asignar_var((X, Y), MI, MF), palabras_aux([XS | XSS], [YS | YSS], MF).

/*5- Otra resolución:
palabras_con_variables([],[]).
palabras_con_variables(XSS,YSS) :- append(XSS,VS), asignaciones(VS,CVS), aux2(XSS,CVS,YSS).

asignaciones([X],M) :- asignar_var(X,[],M).
asignaciones([X|XS],M2) :- asignaciones(XS,R), asignar_var(X,R,M2).   

aux2([],_,[]).
aux2([XS|XSS],CVS,YSS) :- aux3(XS,CVS,R), aux2(XSS,CVS,RSS), append([R],RSS,YSS).

aux3([],_,[]).
aux3([X|XS],CVS,YS) :- member((X,Z),CVS), aux3(XS,CVS,RS), append([Z],RS,YS).
%%ver si se puede resolver de otra manera*/

% 6- quitar(E, L, R)
quitar(_, [], []).
quitar(A, [B | XS], YS) :- A == B, quitar(A, XS, YS).
quitar(A, [B | XS], R) :-  A \= B, nonvar(A), nonvar(B), quitar(A, XS, YS), append([B],YS,R).
quitar(A, [B | XS], R) :-  A \== B, var(A), var(B), quitar(A, XS, YS), append([B],YS,R).
quitar(A, [B | XS], [B|YS]) :- var(A), nonvar(B), quitar(A, XS, YS).    %%ver si se pueden meter estas 2 en 1 sola.
quitar(A, [B | XS], [B|YS]) :- nonvar(A), var(B), quitar(A, XS, YS).

%7-cant_distintos(L, S)
%% cant_distintos([],0).
%% cant_distintos([X|XS],N) :- not(member(X,XS)), cant_distintos(XS,M), N is M+1.  
%% cant_distintos([X|XS],M) :- member(X,XS), cant_distintos(XS,M). 

cant_distintos([],0).
cant_distintos([_],1).
cant_distintos([X|XS],N) :- quitar(X,XS,R), cant_distintos(R,M), N is M+1.


% PREGUNTAR cant_distintos([A, B, A], N) -> N = 2? y si A = B?
% PREGUNTAR - qué significa que algo puede no instanciarse? simplemente que, al no instanciarse, no se cuegla?
% PREGUNTAR - como justificamos las instanciaciones?
% como justificamos que palabras(S, P) se cuelga? 