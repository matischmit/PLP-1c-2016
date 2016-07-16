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

ej(5, [rombo, espacio, cuadrado, espacio, sol, espacio, luna]).

ej(6, [rombo, cuadrado, sol, luna]).


%%%%%%%%%%% TESTS %%%%%%%%%%%%
%TODOS
tests :- test_juntar_con, test_juntar_con2, test_juntar_con3, test_palabras, test_palabras2, test_asignar_var, test_asignar_var2,
         test_palabras_con_variables, test_palabras_con_variables2, test_quitar, test_quitar2, test_cant_distintos,
         test_cant_distintos2, test_cant_distintos3, test_descifrar, test_descifrar2, test_descifrar3, test_descifrar4,
         test_descifrar_sin_espacios,test_descifrar_sin_espacios2, test_descifrar_sin_espacios3,
         test_mensajes_mas_parejos, test_mensajes_mas_parejos2, test_mensajes_mas_parejos3.

%2
test_juntar_con :- juntar_con([[rombo], [cuadrado], [sol], [luna]], espacio, [rombo, espacio, cuadrado, espacio, sol, espacio, luna]).
test_juntar_con2 :- juntar_con([[rombo, cuadrado], [sol], [luna]], espacio, [rombo, cuadrado, espacio, sol, espacio, luna]).
test_juntar_con3 :- juntar_con([], _, []).

%3
test_palabras :- palabras([a, a, a, a, espacio, b, b, b, b, espacio, c, c, c], [[a,a,a,a], [b,b,b,b], [c,c,c]]).
test_palabras2 :- palabras([], []).

%4
test_asignar_var :- asignar_var(a, [(a, A), (b, B), (c, C), (d, D)], [(a, A), (b, B), (c, C), (d, D)]).
test_asignar_var2 :- asignar_var(a, [(b, B), (c, C), (d, D)], [(a, _), (b, B), (c, C), (d, D)]).

%5
test_palabras_con_variables:- palabras_con_variables([[a,a],[b,b],[c,c,c]], [[A,A], [B,B], [C,C,C]]).
test_palabras_con_variables2:- palabras_con_variables([], []).

%6
test_quitar :- quitar(a,[A,A,A, B, a, B, B],[A,A,A, B, B, B]).
test_quitar2 :- quitar(A, [A,A,A, B, a, B, B], [B, a, B, B]).

%7
test_cant_distintos :- cant_distintos([A,_,A,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C], 3).
test_cant_distintos2 :- cant_distintos([], 0).
test_cant_distintos3 :- cant_distintos([A,A,A,A,A,a,a,a,a,a], 2).

%8
test_descifrar :- cargar('dicc0.txt'), descifrar([rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado], 'la casa').
test_descifrar2 :- cargar('dicc0.txt'), descifrar([cuadrado, rombo, espacio, perro, rombo, sol, rombo], 'la casa').
test_descifrar3 :- cargar('dicc0.txt'), descifrar([cuadrado, rombo, espacio, perro, circulo, sol, rombo], 'la cosa').
test_descifrar4 :- cargar('dicc1.txt'), descifrar([m, i, e, n, t, o, espacio, d, e], 'miento de').
test_descifrar_largo :- cargar('10000_formas.txt'), time(descifrar([t,a,s,a,s,espacio,l,e,v,a,n,t,a,espacio,c,u,a,r,t,a,espacio,t,a,z,a,espacio,c,o,n,c,l,u,s,i,o,n,e,s,espacio,n,e,c,e,s,i,t,a,n,espacio,n,e,g,a,t,i,v,a,espacio,d,i,e,t,a,espacio,a,l,c,a,n,z,a,d,o,espacio,a,n,d,a,r],'tasas levanta cuarta taza conclusiones necesitan negativa dieta alcanzado andar')).

%9
test_descifrar_sin_espacios :- cargar('dicc0.txt'), descifrar_sin_espacios([l,a,c,a,s,a], 'la casa').
test_descifrar_sin_espacios2 :- cargar('dicc0.txt'), descifrar_sin_espacios([l,a,c,o,s,a], 'la cosa').
test_descifrar_sin_espacios3 :- cargar('dicc0.txt'), descifrar_sin_espacios([c,a,s,a,c,o,s,a], 'casa cosa').

%10
test_mensajes_mas_parejos :- cargar('dicc1.txt'), mensajes_mas_parejos([c,a,s,a,d,e,f,l,o,r], 'casa de flor').
test_mensajes_mas_parejos2 :- cargar('dicc1.txt'), mensajes_mas_parejos([c,a,s,a,d,e,f,l,o,r], 'casa flor de').
test_mensajes_mas_parejos2 :- cargar('dicc1.txt'), mensajes_mas_parejos([d,e,f,l,o,r,c,a,s,a], 'de flor casa').
test_mensajes_mas_parejos3 :- cargar('dicc1.txt'), mensajes_mas_parejos([d,e,f,l,o,r,c,a,s,a], 'flor de casa').

% IMPORTANTE: No se permite usar findall, setof ni cuts. Sí se permite not.

%% 1 - diccionario_lista(?L)
diccionario_lista(X) :- diccionario(S), string_codes(S,X).

%% 2 - juntar_con(?L, ?J, ?R)
%% Con la instanciación +L ?J -R, genera el resultado esperado de unir los elementos de L con J. Si J no está instanciado los deja en la lista sin instanciar, como se esperaría.
%% Con -L +J +R, genera varios resultados posibles, ya que podría ser que J pertenezca a las listas de L, o que hayan sido agregadas posteriormente.
%%         Por ejemplo: "juntar_con(L1,b,[a,b,c])." tiene dos resultados: L1 = [[a, b, c]] ; L1 = [[a], [c]] ;
%% Con -L -J +R, instancia J para cada elemento "elem" de R, y genera los resultados correspondientes a juntar_con(L,elem,R) para cada uno de ellos.
%% Con -L ?J -R, se queda instanciando L en una lista de n listas vacías, y a R en u
juntar_con([X], _, X).
juntar_con([X, Y | Ltail], J, R) :- append(X, [J | LtailRec], R), juntar_con([Y | Ltail], J, LtailRec).
juntar_con([], _, []).

%% 3 - palabras(?S, ?P)
%% OBS: No pueden estar ambas sin instanciar, porque los member de tiene_espacio "fuerzan" que haya
%%      un espacio en una lista de P, y nunca puede pasar el not(tiene_espacio(P)).
tiene_espacio(P) :- member(L, P), member(espacio, L).
palabras(S, P) :- juntar_con(P, espacio, S), not(tiene_espacio(P)).

%% 4 - asignar var(?A, ?MI, ?MF)
%% OBS: A siempre puede estar instanciada o sin instanciar.
%%      Si ambas MI y MF no están instanciadas, siempre cae en el primer caso, A pertenece a MI, MI es igual a MF,
%%      y genera las MI como listas con (a, VAR) con miembro.
%% Preguntamos y el orden NO importa, y sólo tiene que devolver una opción
asignar_var(A, MI, MI) :- member((A,C), MI).
asignar_var(A, MI, [(A,C)| MI]) :- not(member((A,C), MI)).

%% 5 - palabras con variables(+P, ?V)
%% OBS: Si P no está instanciado no se cuelga, pero no es capaz de generar átomos.
palabras_con_variables(XSS, YSS) :- palabras_aux(XSS, YSS, []).

palabras_aux([], [], _).
palabras_aux([[] | XSS], [[] | YSS], M) :- palabras_aux(XSS, YSS, M).
palabras_aux([[X | XS] | XSS], [[Y | YS] | YSS], MI) :- asignar_var((X, Y), MI, MF), palabras_aux([XS | XSS], [YS | YSS], MF).

%% 6 - quitar(?E, +L, ?R)
%% OBS: No es reversible en L, porque en caso de no instanciar L no genera todas las listas que tengan a E como elemento.
quitar(_, [], []).
quitar(A, [B|XS], YS) :- A == B,  quitar(A, XS, YS).
quitar(A, [B|XS], [B|YS]) :- A \== B, quitar(A, XS, YS).


%% 7 - cant_distintos(+L, ?S)
%% OBS: Hay que instanciar L porque quitar necesita a L instanciado
cant_distintos([],0).
cant_distintos([X|XS],N) :- quitar(X,XS,R), cant_distintos(R,M), N is M+1.


%% 8 - descifrar(+S, ?M)
%% OBS: S tiene que estar instanciado porque se llama a cant_distintos(S, DistS) y cant_distintos requiere que S este instanciada
descifrar(S, M) :- palabras(S, P), palabras_con_variables(P, V),
                   unificar(V, N), juntar_con(N, 32, Nflat),
                   cant_distintos(S, DistS), cant_distintos(Nflat, DistNflat),
                   DistS = DistNflat, string_codes(M, Nflat).


% unifica una lista de listas de variables libres con palabras del diccionario
unificar([], []).
unificar([X | TailX], [Y | TailY]) :- diccionario_lista(Y), Y=X, cant_distintos(Y, DistY), cant_distintos(X, DistX),
                                      DistY = DistX, unificar(TailX, TailY).



%% 9 - descifrar_sin_espacios(+S, ?M)
%% S tiene que estar instanciada porque, de lo contrario, agregar_espacios(S, Sesp) instancia en Sesp listas de variables libres y espacios.
%% Al intentar descifrar eso, se llama a palabras(Sesp, P). palabras llama a juntar_con que divide por espacios y pide not(tiene_espacios(P)).
%% el not prueba todas las instanciaciones posibles y falla siempre porque puede unificar las variables libres con espacios.
descifrar_sin_espacios(S, M) :- agregar_espacios(S, Sesp), descifrar(Sesp, M).
notempty([_|_]).

agregar_espacios(S, S).
agregar_espacios(S, Sesp) :- append(S1, S2, S), notempty(S1), notempty(S2), agregar_espacios(S2, S2esp), juntar_con([S1, S2esp], espacio, Sesp) .

%% 10 - mensajes_mas_parejos(+M, ?X)
%% M tiene que estar instanciado porque descifrar_sin_espacios(M,X) requiere que M esté instanciado.
mensajes_mas_parejos(M,X) :-  descifrar_sin_espacios(M,X), not(uno_mas_parejo(M,X)).

uno_mas_parejo(M,X) :- descifrar_sin_espacios(M,Y), X \= Y, sd(X,Z), sd(Y,Q), Z > Q.


sd(S,Q) :- string_codes(S, SLista), juntar_con(LP, 32, SLista), not(tiene_caracter_espacio(LP)), map_length(LP,LT) , mean(LT,M), length(LT,N), calculo(LT,M,Y), Z is Y/N, Q is sqrt(Z)  .

% tiene_caracter_espacio(?P). Es idéntica a 'tiene_espacio', excepto que busca el caracter espacio (32) en vez del atomo 'espacio'.
% No quisimos abstraer ambas en una sola funcion para no ensuciar la resolución del ejercicio 3.
tiene_caracter_espacio(P) :- member(L,P), member(32,L).

% Otra solución para sd: sd(S,Q) :- atomic_list_concat(LP, ' ', S), map_length(LP,LT) , mean(LT,M), length(LT,N), calculo(LT,M,Y), Z is Y/N, Q is sqrt(Z)  .
% No lo resolvimos de esta forma porque nos pareció mejor aprovechar las funciones que tenemos hechas (juntar_con) en vez de las de Prolog (atomic_list_concat)

%Funcion map de longitud en lista
map_length([],[]).
map_length([X|XS],Y) :- string_length(X,R), map_length(XS,RS), append([R],RS,Y).

%Media de lista
mean(XS,Y) :- length(XS,N), sumlist(XS,S), Y is S/N.

% Sumas de cuadrados de diferencia con la media (desv.estandar sin la division y el sqrt)
calculo([],_,0).
calculo([X|XS],M,Q) :- Y is (X-M)^2, calculo(XS,M,Z), Q is Y+Z .
