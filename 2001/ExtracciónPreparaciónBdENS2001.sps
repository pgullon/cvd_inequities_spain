
DATA LIST FILE= 'C:\Users\usuario\Desktop\Trabajo\Tesis\Datos\ENS_España\ENS 2001\ADULTO2001\ADULTO01.txt'
/NESTUDIO 1-4 NCUEST 5-9 CCAA 10-11 PROVINCIA 12-13 MUNICIPIO 14-16 TMUNI 17 METROPOLI 18 DISTRITO 19-20 
SECCION 21-23 NENTREV 24-27 SUBMUESTRA1 28 BLOQUE 29-30 CUESTADULT 31 
P1 40-41 P2_01 42-43 P3_01 44 P4_01 45-46 P2_02 47-48 P3_02 49 P4_02 50-51 P2_03 52-53 P3_03 54 P4_03 55-56 P2_04 57-58 P3_04 59 P4_04 60-61
P2_05 62-63 P3_05 64 P4_05 65-66 P2_06 67-68 P3_06 69 P4_06 70-71 P2_07 72-73 P3_07 74 P4_07 75-76 P2_08 77-78 P3_08 79 P4_08 80-81
P2_09 82-83 P3_09 84 P4_09 85-86 P2_10 87-88 P3_10 89 P4_10 90-91 
P5 92 P5A1 93-94 P5A2 95-96  P5B 97-98 P6 99-100 P6A 101 P7 102-103 P8 104 P9 105 P10 106 P10A 107-110 P11_1 TO P11_10 111-120 P11A 121 
P12 122 P12A 123 P12B 124 P12C1 125 P12C2 126 P13 127 P13A 128-129 P13B_1 TO P13B_19 130-148 P14 153 P14A 154-155 P14B_1 TO P14B_19 156-174
P15 179 P15A 180-181 P16 182 P16A_1 TO P16A_19 183-201 P16B_1 TO P16B_19 205-223 P17 227 P17A_1 228-229 P17A_2 230-231 P18 232-233 P18A 234-235 
P18B 236 P18C 237-238 P18D 239-241 P18E 242-244 P18F 245 P19 246 P19A 247-248 P20_1 249-250 P20_2 251-252 P21_1 TO P21_10 253-262 P22 263 
P23_1 TO P23_9 264-272 P24 273 P24A 274 P24B 275-277 P24C 278 P24D 279 P24E 280-281 P24F 282 P24G 283 P25 284 P25A 285-286 P25B 287 P25C 288
P26 289 P26A 290-291 P27 292-293 P28 294 P29_1 295-296 P29_2 297-298 P29_3 299-300 P29A 303 P30 304 P30A_1 305-306 P30A_2 307-308 P30A_3 309-310
P30B 311-312 P30C 313 P31 314-315 P31A_1 316-317 P31A_2 318-319 P31B 320-321 P31C_1 322-323 P31C_2 324-325 P31C_3 326-327 P32_328 
P33_1 329-330 P34_1 331 P33_2 332-333 P34_2 334 P33_3 335-336 P34_3 337 P33_4 338-339 P34_4 340 P33_5 341-342 P34_5 343 P33_6 344-345 P34_6 346 
P35_1 TO P35_6 347-352 P36 353-354 P37_1 TO P37_7 355-361 P38 362-363 P39 364 P40 365 P41 366 P42_1 TO P42_10 367-376 P43 377 P43A 378 P44 379 P44A 380 P44B 381
P45 382-384 P46 385-387 P47 388 P48 389 P48A 390 P49 391 P49A 392 P50 393-394 P51_1 TO P51_27 395-421 SEXO 435 P56 436 P56A 437 P57 438 P57A 439-440 P58 441 P58A 442
P59 443-445 P60 446 P60A 447 P61 448-449 P62 450 P63 451 P64 452 P64A 453-454 P65 455 P66 456-458 P67 459 P67A 460 P68 461-462 P69 463 P70 464 
I1 465-466 I2 467-468 I3 469-470 I4 471-472 I5 473-474 I6 475-476 I7 477-478 I8 479-480 I9 481-482 E1D 483-484 E1M 485-486 E1A 487-488 E2 489 E3 490-492 E4 493 V1 494 V2 495 
C1 496 C1A 497-498 C2 499 C2A 500 C2B 501-502 C3 503 C4 504-505 FACTOR 506-510. 
EXECUTE. 



* Definir propiedades de variables.
*NESTUDIO.
VARIABLE LABELS  NESTUDIO 'Nº de estudio'.
*NCUEST.
VARIABLE LABELS  NCUEST 'Nº encuesta'.
EXECUTE.

*CCAA.
VARIABLE LABELS  CCAA ' Región de Residencia'.
VALUE LABELS CCAA
  1 'Andalucía'
  2 'Aragón'
  3 'Asturias'
  4 'Baleares'
  5 'Canarias'
  6 'Cantabria'
  7 'Castilla la Mancha'
  8 'Castilla León'
  9 'Cataluña'
  10 'Comunidad Valenciana'
  11 'Extremadura'
  12 'Galicia'
  13 'Madrid'
  14 'Murcia'
  15 'Navarra'
  16 'País Vasco'
  17 'La Rioja'
  18 'Ceuta'
  19 'Melilla'.

 Definir propiedades de variables.
*PROVINCIA.
VARIABLE LABELS  PROVINCIA 'provincia'.
*MUNICIPIO.
VARIABLE LABELS  MUNICIPIO 'municipio'.
*TMUNI.
VARIABLE LABELS  TMUNI 'Tamaño del municipio '.
VALUE LABELS TMUNI
  1 '>=2000'
  2 '2001 a 10000'
  3 '10001 a 50000'
  4 '50001 a 100000'
  5 '100001 a 400000'
  6 '400001 a 1000000'
  7 '>1000000'.
*METROPOLI.
VARIABLE LABELS  METROPOLI 'Area metropolitana'.
VALUE LABELS METROPOLI
  0 'no'
  1 'si'.
*P1.
VARIABLE LABELS  P1 'Nº personas que viven habitualmente en la vivienda'.
*P2_01.
VARIABLE LABELS  P2_01 'Parentesco del entrevistado'.
VALUE LABELS P2_01
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'.
*P3_01.
VARIABLE LABELS  P3_01 'Sexo del entrevistado'.
VALUE LABELS P3_01
  1 'Hombre'
  2 'Mujer'.
*P4_01.
VARIABLE LABELS  P4_01 'EDAD del entrevistado'.
*P2_02.
VALUE LABELS P2_02
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  9 'Otros no familiares'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  99 'NS/NC'.
*P3_02.
VALUE LABELS P3_02
  1 'Hombre'
  2 'Mujer'.
*P2_03.
VALUE LABELS P2_03
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'.
*P3_03.
VALUE LABELS P3_03
  1 'Hombre'
  2 'Mujer'.
*P2_04.
VALUE LABELS P2_04
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'.
*P3_04.
VALUE LABELS P3_04
  1 'Hombre'
  2 'Mujer'.
*P2_05.
VALUE LABELS P2_05
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'.
*P3_05.
VALUE LABELS P3_05
  1 'Hombre'
  2 'Mujer'.
*P2_06.
VALUE LABELS P2_06
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'.
*P3_06.
VALUE LABELS P3_06
  1 'Hombre'
  2 'Mujer'.
*P2_07.
VALUE LABELS P2_07
  1 'Cabeza de familia'
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  5 'Abuelo'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'.
*P3_07.
VALUE LABELS P3_07
  1 'Hombre'
  2 'Mujer'.
*P2_08.
VALUE LABELS P2_08
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'
  1 'Cabeza de familia'
  5 'Abuelo'.
*P3_08.
VALUE LABELS P3_08
  1 'Hombre'
  2 'Mujer'.
*P2_09.
VALUE LABELS P2_09
  2 'Pareja'
  3 'Hijo'
  4 'Padre/suegro'
  6 'Nieto'
  7 'Hermano'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'
  1 'Cabeza de familia'
  5 'Abuelo'.
*P3_09.
VALUE LABELS P3_09
  1 'Hombre'
  2 'Mujer'.
*P2_10.
VALUE LABELS P2_10
  3 'Hijo'
  4 'Padre/suegro'
  6 'Nieto'
  8 'Otros familiares'
  9 'Otros no familiares'
  99 'NS/NC'
  1 'Cabeza de familia'
  2 'Pareja'
  5 'Abuelo'
  7 'Hermano'.
*P3_10.
VALUE LABELS P3_10
  1 'Hombre'
  2 'Mujer'.
*P5.
VARIABLE LABELS  P5 'Dedicación especial por minusvalía'.
VALUE LABELS P5
  1 'Sí'
  2 'No'
  9 'NC'.
*P5A1.
VARIABLE LABELS  P5A1 'Años persona con minusvalía'.
*P5A2.
VARIABLE LABELS  P5A2 'Años persona con minusvalía'.
*P5B.
VARIABLE LABELS  P5B 'quién se ocupa de persona con minusvalía'.
*P6.
VARIABLE LABELS  P6 'quién se ocupa de los niños'.
*P6A.
VARIABLE LABELS  P6A 'viene alguien para el cuidado de niños'.
VALUE LABELS P6A
  1 'sí'
  2 'no'
  9 'NC'.
*P7.
VARIABLE LABELS  P7 'quién se ocupa de la casa'.
*P8.
VARIABLE LABELS  P8 'ayuda con trabajos domésticos'.
VALUE LABELS P8
  1 'sí'
  2 'no'
  9 'nc'.
*P9.
VARIABLE LABELS  P9 'Salud percibida últimos 12 meses'.
VALUE LABELS P9
  1 'Muy bueno'
  2 'Bueno'
  3 'Regular'
  4 'Malo'
  5 'Muy malo'
  8 'NS'
  9 'NC'.
*P10.
VARIABLE LABELS  P10 'Enfermedad limitante 10d los últimos 12 meses'.
VALUE LABELS P10
  1 'Sí'
  2 'No'
  9 'NS/NC'.
*P11_1.
VARIABLE LABELS  P11_1 'Dx médico de HTA'.
VALUE LABELS P11_1
  1 'Sí'
  2 'No'.
*P11_2.
VARIABLE LABELS  P11_2 'Dx médico Colesterol'.
VALUE LABELS P11_2
  1 'Sí'
  2 'No'.
*P11_3.
VARIABLE LABELS  P11_3 'Dx médico DM'.
VALUE LABELS P11_3
  1 'Sí'
  2 'No'.
*P11_4.
VARIABLE LABELS  P11_4 'Dx médico asma'.
VALUE LABELS P11_4
  1 'Sí'
  2 'No'.
*P11_5.
VARIABLE LABELS  P11_5 'Dx médico enfermedades corazón'.
VALUE LABELS P11_5
  1 'Sí'
  2 'No'.
*P11_6.
VARIABLE LABELS  P11_6 'Dx médico úlcera de estómago'.
VALUE LABELS P11_6
  1 'Sí'
  2 'No'.
*P11_7.
VARIABLE LABELS  P11_7 'Dx médico alergia'.
VALUE LABELS P11_7
  1 'Sí'
  2 'No'.
*P11_8.
VARIABLE LABELS  P11_8 'Dx médico depresión'.
VALUE LABELS P11_8
  1 'Sí'
  2 'No'.
*P11_9.
VALUE LABELS P11_9
  1 'Sí'
  2 'No'.
*P11_10.
VALUE LABELS P11_10
  1 'Sí'
  2 'No'.
*P11A.
VARIABLE LABELS  P11A 'Limitación por alguna enf crónica los últimos 12m'.
VALUE LABELS P11A
  1 'Sí'
  2 'no'
  9 'NC'.
*P12.
VARIABLE LABELS  P12 'Accidente últimos 12m'.
VALUE LABELS P12
  1 'Sí'
  2 'no'
  9 'NC'.
*P13.
VARIABLE LABELS  P13 'Limitación actividades aire libre por enf en últ 15d'.
VALUE LABELS P13
  1 'Sí'
  2 'No'
  9 'NS/NC'.
*P14.
VARIABLE LABELS  P14 'Limitación actividad ppal por enf últ 15d'.
VALUE LABELS P14
  1 'Sí'
  2 'No'
  9 'NS/NC'.
*P15.
VARIABLE LABELS  P15 'Quedarse en cama en últimos 15d'.
VALUE LABELS P15
  1 'sí'
  2 'no'
  9 'ns'.
*P16.
VARIABLE LABELS  P16 'uso de medicamentos últimos 15 d'.
VALUE LABELS P16
  1 'sí'
  2 'no'
  9 'ns'.
*P16B_19.
VALUE LABELS P16B_19.
*P17.
VARIABLE LABELS  P17 'consulta médica últimos 15d'.
VALUE LABELS P17
  1 'sí'
  2 'no'
  9 'ns'.
*P19.
VARIABLE LABELS  P19 'Dentista últimos 3 meses'.
VALUE LABELS P19
  1 'sí'
  2 'no'
  9 'nc'.
*P24.
VARIABLE LABELS  P24 'Ha estado hospitalizado en los últ 12m'.
VALUE LABELS P24
  1 'sí'
  2 'no'
  9 'nc'.
*P25.
VARIABLE LABELS  P25 'uso servicio de urgencias últimos 12m'.
VALUE LABELS P25
  1 'sí, acudí'
  2 'sí, vinieron'
  3 'no'
  9 'nc'.
*P45.
VARIABLE LABELS  P45 'Peso (kg)'.
*P46.
VARIABLE LABELS  P46 'altura (cm)'.
*P50.
VARIABLE LABELS  P50 'EDAD'.
*P57.
VARIABLE LABELS  P57 'Ha ido a escuela'.
VALUE LABELS P57
  1 'Analfabeto'
  2 'Sabe leer y escribir pero no escuela'
  3 'Ha ido a escuela'.
*P57A.
VARIABLE LABELS  P57A 'Nivel de estudios más alto'.
VALUE LABELS P57A
  0 'No escuela'
  1 'Algunos años est primarios'
  2 'Estudios primarios'
  3 'Bach elemental'
  4 'FP'
  5 'Bach superior'
  6 'FP2'
  7 'Diplomaturas'
  8 'Otros diplomados'
  9 'Estudios superiores'
  10 'Arquitecto, ingeniero sup'
  11 'Licenciado'
  12 'Doctorado'
  13 'Postgrados, masters'
  14 'Otros est no reglados'
  99 'nc'.
*P58.
VARIABLE LABELS  P58 'Situación laboral'.
VALUE LABELS P58
  1 'Trabaja'
  2 'Jubilado'
  3 'Pensionista sin trabajo previo'
  4 'Parado con trabajo previo'
  5 'Parado no ha trabajado antes'
  6 'Estudiante'
  7 'Sus labores'
  8 'Otras'
  9 'Nc'.
*P58A.
VARIABLE LABELS  P58A 'Ha trabajado antes?'.
VALUE LABELS P58A
  1 'sí'
  2 'no'
  9 'nc'.
*P60.
VARIABLE LABELS  P60 'trabaja/ba como'.
VALUE LABELS P60
  1 'asalariado fijo'
  2 'asalariado eventual'
  3 'empresario con asalariados'
  4 'autónomo'
  5 'ayuda familiar'
  6 'miembro cooperativa'
  7 'otra'
  9 'nc'.
*P69.
VARIABLE LABELS  P69 'Ingresos totales hogar'.
VALUE LABELS P69
  1 '<60000  pts'
  2 '60001 a 100000 pts'
  3 '100001 a 150001'
  4 '150001 a 200000'
  5 '200001 a 300000'
  6 '>300000'
  9 'nc'.
EXECUTE.





******************************************************************************
PONDERACIÓN
*****************************************************************************


COMPUTE FactorN25= FACTOR *1.004059.
EXECUTE.

COMPUTE FactorNHombre25= FACTOR *1.0061.
EXECUTE.

COMPUTE FactorNMujer25= FACTOR *1.0018388.
EXECUTE.