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
% OBS: No pueden estar ambas sin instanciar, porque los member de tieneEspacio "fuerzan" que haya
%      un espacio en una lista de P, y nunca puede pasar el not(tieneEspacio(P)).
tieneEspacio(P) :- member(L, P), member(espacio, L).
palabras(S, P) :- juntar_con(P, espacio, S), not(tieneEspacio(P)).

% 4- asignar var(A, MI, MF)
% OBS: Preguntamos y el orden NO importa, y sólo tiene que devolver una opción
asignar_var(A, [], [(A, _)]).
asignar_var(A, [(B, C) | LS], [(B, C) | LS2]) :- asignar_var(A, LS, LS2), A \= B.
asignar_var(A, [(A, C) | LS], [(A, C) | LS2]) :- LS=LS2.


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

%% 8 - descifrar(S+, M?)

% unifica una lista de listas de variables libres con palabras del diccionario
unificar([], []).
unificar([X | TailX], [Y | TailY]) :- diccionario_lista(Y), Y=X, unificar(TailX, TailY).

descifrar(S, M) :- palabras(S, P), palabras_con_variables(P, V), unificar(V, N), juntar_con(N, 0'\s, Nflat), string_codes(M, Nflat).

%10 - mensajes_mas_parejos(S, M)

foo(S) :- member(S,["casa miento", "casa de flor", "casa flor de"]).  %Resultado de ej(3, S), descifrar_sin_espacios(S, M).

mensajes_mas_parejos(W) :- findall(M, foo(M), X), min_sd(X,W).  %TODO: cambiar foo por descifrar_sin.., ver si se puede evitar usar findall.

min_sd([X],X).
min_sd([X|XS],X) :- min_sd(XS,Y), sd(X,Z), sd(Y,Q), Z=<Q.
min_sd([X|XS],Y) :- min_sd(XS,Y), sd(X,Z), sd(Y,Q), Z>=Q.

sd(S,Q) :- atomic_list_concat(LP, " ", S), map_length(LP,LT) , mean(LT,M), length(LT,N), calculo(LT,M,Y), Z is Y/N, Q is sqrt(Z)  .

%% lista_palabras_en_frase(S,LP) :- atomic_list_concat(LP, " ", S).

%Funcion map de longitud en lista
map_length([],[]).
map_length([X|XS],Y) :- string_length(X,R), map_length(XS,RS), append([R],RS,Y).

%Media de lista
mean(XS,Y) :- length(XS,N), sumlist(XS,S), Y is S/N.

% Sumas de cuadrados de diferencia con la media (desv.estandar sin la division y el sqrt)
calculo([],_,0).
calculo([X|XS],M,Q) :- Y is (X-M)^2, calculo(XS,M,Z), Q is Y+Z .
