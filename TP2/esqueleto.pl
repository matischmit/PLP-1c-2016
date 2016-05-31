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
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

esp(espacio). % no estoy seguro de como usar espacio si no es de esta forma


%%%%%%%%5

% 2- juntar_con(L, J, R)
juntar_con([X | Ltail], J, R) :- append(X, [J | LtailRec], R), juntar_con(Ltail, J, LtailRec).
juntar_con([], J, []).

% 3- palabras(S, P)
% VER - no anda bien, anida las listas, por ejemplo anda para el caso:
% palabras([rombo, cuadrado, espacio, cuadrado, rombo, espacio, rombo], P). -> tira P = [[rombo, cuadrado], [[cuadrado, rombo], [rombo]]]
% palabras(S, P) :- append(Spref, [espacio | Ssuf], S), append([P1], [P2], P), palabras(Spref, P1), palabras(Ssuf, P2).

%palabras([espacio | LS], [XS | XSS]) :- palabras(LS, XSS), !.
%palabras([X | LS], [[X | XS] | XSS]) :- palabras(LS, [XS | XSS]).
%palabras([], [[]]).


palabras([X | [espacio | LS]], [[X] | XSS]) :- palabras(LS, [XS | XSS]), !.
palabras([X | LS], [[X |XS] | XSS]) :- palabras(LS, XSS).
palabras([], []).