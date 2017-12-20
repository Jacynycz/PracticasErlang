-module (pract4).
-include_lib("eunit/include/eunit.hrl").
-include("pract4.hrl").
-export ([edad/1,vecinos/2,habitantes/2,incluye/2,mapSafe/2,poker/1]).
-define (SUMA,fun(X,Y)-> X+Y end).
-define (SUMA_ABS,fun(X,Y) -> abs(X)+abs(Y) end).



edad(Persona) -> Persona#persona.edad.

vecinos(P1, P2) -> (P1#persona.ciudad == P2#persona.ciudad) andalso (P1#persona.calle == P2#persona.calle).

habitantes(Ps, Ciudad) -> [ X || X <- Ps, X#persona.ciudad == Ciudad].

incluye(P,[]) -> [P];
incluye(P,[H|T]) ->
case P#persona.dni == H#persona.dni of
  true -> [H|T];
  false -> [H|incluye(P,T)]
end.

mapSafe(_,[]) -> [];
mapSafe(F,[X|Xs]) -> [
try F(X)
catch
  error:_-> error
end|mapSafe(F,Xs)].

inc_valor(Carta,[]) -> [{Carta#carta.valor,1}];
inc_valor(Carta,[H|T]) -> {V,X} = H,case V == Carta#carta.valor of
  true -> [{Carta#carta.valor,X+1}|T];
  false -> [H|inc_valor(Carta,T)]
end.

comprueba4([])  -> false;
comprueba4([H|T])  -> {_,X} = H, case X == 4 of
  true -> true;
  false -> comprueba4(T)
end.

poker_mano([],L) -> L;
poker_mano([Carta|Mano],L) ->poker_mano(Mano,inc_valor(Carta,L)).

poker(M) -> Mano = M#mano.cartas, L=poker_mano(Mano,[]),io:write(L),comprueba4(L).

eje1Mano_test() -> _ = #mano{cartas=[#carta{valor=2,palo=corazones},
                                    #carta{valor=3,palo=rombos},
                                    #carta{valor=as,palo=tréboles},
                                    #carta{valor=2,palo=rombos},
                                    #carta{valor=3,palo=picas}
                                   ]
                            }.

eje1Valores_test() -> ?assertEqual(?VALORES,[as,2,3,4,5,6,7,8,9,10,j,q,k]).

eje1Persona_test() -> _ = #persona{edad=25,nombre="bertoldo",apellidos="cacaseno",dni=1,calle="C/Jazmin",ciudad="Madrid"}.

edad_test() -> P = eje1Persona_test(),
               ?assertEqual(pract4:edad(P),25).


 p() -> [eje1Persona_test(), #persona{edad=28,nombre="herminia",apellidos="Filón",
                                       dni=2,calle="C/Jazmin",ciudad="Madrid"}].

 vecinosSI_test() -> [P1,P2] = p(),
                   ?assert(pract4:vecinos(P1,P2)).

 vecinosNO_test() -> [P1,P2] = p(),
                   ?assertNot(pract4:vecinos(P1,P2#persona{ciudad="Lugo"})).

 habitantes_test() -> [P1,P2] = p(),
                       ?assertEqual(pract4:habitantes([P1,
                                      P1#persona{ciudad="Lugo"},P2],"Madrid"),
                                    [#persona{nombre = "bertoldo",apellidos = "cacaseno",
                                     dni = 1,edad = 25,calle = "C/Jazmin",ciudad = "Madrid"},
                                     #persona{nombre = "herminia",apellidos = "Filón",dni = 2,
                                     edad = 28,calle = "C/Jazmin",ciudad = "Madrid"}] ).

incluye_test() -> [P1,P2] = p(),
                 ?assertEqual(pract4:incluye(P1,[P1,P2]),
[#persona{nombre = "bertoldo",apellidos = "cacaseno",
         dni = 1,edad = 25,calle = "C/Jazmin",ciudad = "Madrid"},
#persona{nombre = "herminia",apellidos = "Filón",dni = 2,
         edad = 28,calle = "C/Jazmin",ciudad = "Madrid"}]),
                 ?assertEqual(pract4:incluye(P1,[P2]),
[#persona{nombre = "herminia",apellidos = "Filón",dni = 2,
         edad = 28,calle = "C/Jazmin",ciudad = "Madrid"},
 #persona{nombre = "bertoldo",apellidos = "cacaseno",
        dni = 1,edad = 25,calle = "C/Jazmin",ciudad = "Madrid"}] ).

 map_test() -> ?assertEqual(pract4:mapSafe(fun(X)->1/X end,[1,2,3,4,0,5]),
                              [1.0,0.5,0.3333333333333333,0.25,error,0.2]).


                            pokerSI_test() -> ?assert(pract4:poker(#mano{cartas=[#carta{valor=3,palo=corazones},
                                                                #carta{valor=3,palo=rombos},
                                                                #carta{valor=as,palo=tréboles},
                                                                #carta{valor=3,palo=tréboles},
                                                                #carta{valor=3,palo=picas}
                                                               ] })).

                            pokerNO_test() -> ?assertNot(pract4:poker(#mano{cartas=[#carta{valor=3,palo=corazones},
                                                                #carta{valor=3,palo=rombos},
                                                                #carta{valor=as,palo=tréboles},
                                                                #carta{valor=q,palo=tréboles},
                                                                #carta{valor=3,palo=picas}
                                                               ] })).
