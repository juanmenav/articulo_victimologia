prompt_instruccion <- "A partir de lo anterior, quiero que haga un análisis de discurso victimológico, esto es centrando en cómo se habla de la víctima, y extraigas la siguiente información basado exclusivamente en el texto entregado:
- ID del discurso.
- Conexiones: refiere a la conexión semántica de problemas de la victimología con otros tópicos discursivos relevantes. Elige máximo 5 opciones que identifiques más claramente, pueden ser menos. Separa con punto y coma cada opción. 
0 = Sin conexiones claras.
1 = Crisis de seguridad:	Se refiere a conexiones entre la situación de las víctimas y la crisis de seguridad. 
2 = Estándares internacionales:	Se refiere a conexiones entre las víctimas y como se aborda su situación en el concierto internacional.
3 = Garantismo:	Se refiere a conexiones entre la situación de las víctimas y los principios del debido proceso en el sistema penal acusatorio adversarial que son a veces denominados peyorativamente como “garantismo”.
4 = Impunidad:	Se refiere a conexiones entre la situación de las víctimas y la impunidad como una sensación colectiva de injusticia.
5 = Justicia:	Se refiere a conexiones entre la situación de las víctimas e ideas de justicia.
6 = Olvido de la víctima:	Se refiere a conexiones entre la situación de las víctimas y cómo son dejadas de lado por la sociedad y el sistema de justicia.
7 = Peligro:	Se refiere a conexiones entre la situación de las víctimas y la percepción de que la sociedad se ha vuelto peligrosa.
8 = Perdida de valores:	Se refiere a conexiones entre la situación de las víctimas y la percepción de que la sociedad ya no se rige por valores como el respeto por el otro.
9 = Prevención:	Se refiere a conexiones entre la situación de las víctimas y nociones sobre prevención del crimen.
10 = Reinserción:	Se refiere a conexiones entre la situación de las víctimas y la reinserción como una función del sistema penal y el castigo.
11 = Mal causado: Se refiere a conexiones entre la situación de la víctima y el daño experimentado a propósito del delito sufrido.
12 = Victimización secundaria: Se refiere a conexiones en la situación de la víctima y daño emocional o psicológico sufrido o exacerbado a propósito de su interacción con instituciones del sistema de justicia.

-Prácticas: Refiere cuando el acto del hablante es interpretable como una práctica socialmente relevante en el contexto socio jurídico determinado. Elige máximo 3 opciones que identifiques más claramente, pueden ser menos. Separa con punto y coma cada opción. 
0 = Sin practicas claras.
1 = Describir la ley:	Refiere a discursos cuya principal función es la descripción objetiva del proyecto de ley en discusión. En estos el hablante evita introducir elementos o valoraciones subjetivas.
2 = Agradecimiento:	Refiere al acto de reconocimiento de la contribución social de un actor y expresiones de agradecimiento.
3 = Toma de posición:	Refiere al acto de posicionarse explicita y políticamente en un determinado asunto divido entre dos o más posiciones. 
4 = Atribuyendo autoría:	Refiere al acto de reclamar autoría material o intelectual de determinado aspecto de la discusión.
5 = Denuncia:	Refiere al acto de denunciar a la opinión pública una situación que se estima grave y desconocida.
6 = Llamado a la acción:	Refiere al acto de convocar a los interlocutores a tomar acciones sobre un determinado asunto.
7 = Ponerse del lado de las víctimas:	Refiere al acto de posicionamiento explícito y político de situarse en favor de las víctimas, frente a un otro.

-Valoración de la víctima:	Refiere a la asignación de bienes sociales desde el concepto de capital social de Bourdieu. Elige máximo 3 opciones que identifiques más claramente, pueden ser menos. Separa con punto y coma cada opción.
0 = Sin valoraciones claras.
1 = Agencia:	Se le reconoce agencia a la víctima en tanto actor con voz propia y capacidad de accionar.
2 = Digno de compasión: Se le reconoce como merecedora de compasión social.
3 = División bueno y malos: Se le asigna a la víctima explícitamente el lugar de lo bueno contra un otro que se le estima malo.
4 = Vulnerable:	Se le asigna una condición de vulnerabilidad frente a un riesgo constante.
5 = Indeseable: Se le asigna un valor negativo a la sola existencia de víctimas en nuestra sociedad, como un producto indeseado de la vida moderna.
6 = Inocente:	Se le niega responsabilidad a la víctima en lo sufrido, se le reconoce la calidad de inocente.
7 = Responsabilidad: Se le asigna responsabilidad a la víctima en lo sufrido, reconociéndole la calidad de culpable.

Al indicar múltiples opciones evita la sobre interpretación del discurso, evita el overfitting.

Genera un archivo JSON con los resultados."

json_estructura <- 'Ahora responde exclusivamente en formato JSON válido. Tu respuesta debe ser ÚNICAMENTE el objeto JSON, comenzando con "{" y terminando con "}". 
Estructura requerida:
{
  "id":"ID del párrafo",
  "conexiones": "0:12 separados por ;",
  "practicas" : "0:7 separados por ;",
  "valoracion": "0:7 separados por ;"
}'
