"""
TESIS - Análisis de resultados
156 respuestas (01/10/23)

"""
# Importo la base de datos
# N156.F01.10

# Convierto la primer columna en el número de ID
N156.F01.10$Marca.temporal = rank(N156.F01.10$Marca.temporal)

# Cambio el nombre de la columna
colnames(N156.F01.10)[colnames(N156.F01.10) == "Marca.temporal"] = "ID"

# Chequeo el nombre de todas las columnas
colnames(N156.F01.10)

# Convierto las respuestas en su equivalente numérico y las guardo en otra columna
#install.packages("dplyr")
library(dplyr)

########################################################################################## 1er parte
# EMOTIONALLY
N156.F01.10 = N156.F01.10 %>%
  mutate(EMOTIONALLY = case_when(
    Si.alguien.sufrió.emocionalmente. == "Para nada relevante (es decir, tal consideración no influye para nada en mi juicio sobre si algo está bien o mal)" ~ 0,
    Si.alguien.sufrió.emocionalmente. == "Para nada relevante" ~ 0,
    Si.alguien.sufrió.emocionalmente. == "No muy relevante" ~ 1,
    Si.alguien.sufrió.emocionalmente. == "Ligeramente relevante" ~ 2,
    Si.alguien.sufrió.emocionalmente. == "Algo relevante" ~ 3,
    Si.alguien.sufrió.emocionalmente. == "Muy relevante" ~ 4,
    Si.alguien.sufrió.emocionalmente. == "Extremadamente relevante" ~ 5,
    Si.alguien.sufrió.emocionalmente. == "Extremadamente relevante (es decir, este uno de los factores más importantes a la hora de juzgar si algo está bien o mal)" ~ 5,
  ))

# TREATED
N156.F01.10 = N156.F01.10 %>%
  mutate(TREATED = case_when(
    Si.algunas.personas.fueron.tratadas.diferente.de.otras. == "Para nada relevante" ~ 0,
    Si.algunas.personas.fueron.tratadas.diferente.de.otras. == "No muy relevante" ~ 1,
    Si.algunas.personas.fueron.tratadas.diferente.de.otras. == "Ligeramente relevante" ~ 2,
    Si.algunas.personas.fueron.tratadas.diferente.de.otras. == "Algo relevante" ~ 3,
    Si.algunas.personas.fueron.tratadas.diferente.de.otras. == "Muy relevante" ~ 4,
    Si.algunas.personas.fueron.tratadas.diferente.de.otras. == "Extremadamente relevante" ~ 5,
  ))

# LOVECOUNTRY
N156.F01.10 = N156.F01.10 %>%
  mutate(LOVECOUNTRY = case_when(
    Si.las.acciones.de.alguien.demostraron.amor.por.su.nación. == "Para nada relevante" ~ 0,
    Si.las.acciones.de.alguien.demostraron.amor.por.su.nación. == "No muy relevante" ~ 1,
    Si.las.acciones.de.alguien.demostraron.amor.por.su.nación. == "Ligeramente relevante" ~ 2,
    Si.las.acciones.de.alguien.demostraron.amor.por.su.nación. == "Algo relevante" ~ 3,
    Si.las.acciones.de.alguien.demostraron.amor.por.su.nación. == "Muy relevante" ~ 4,
    Si.las.acciones.de.alguien.demostraron.amor.por.su.nación. == "Extremadamente relevante" ~ 5,
  ))

# RESPECT
N156.F01.10 = N156.F01.10 %>%
  mutate(RESPECT = case_when(
    Si.alguien.demostró.una.falta.de.respeto.por.la.autoridad. == "Para nada relevante" ~ 0,
    Si.alguien.demostró.una.falta.de.respeto.por.la.autoridad. == "No muy relevante" ~ 1,
    Si.alguien.demostró.una.falta.de.respeto.por.la.autoridad. == "Ligeramente relevante" ~ 2,
    Si.alguien.demostró.una.falta.de.respeto.por.la.autoridad. == "Algo relevante" ~ 3,
    Si.alguien.demostró.una.falta.de.respeto.por.la.autoridad. == "Muy relevante" ~ 4,
    Si.alguien.demostró.una.falta.de.respeto.por.la.autoridad. == "Extremadamente relevante" ~ 5,
  ))

# DECENCY
N156.F01.10 = N156.F01.10 %>%
  mutate(DECENCY = case_when(
    Si.alguien.violó.principios.de.pureza.y.decencia. == "Para nada relevante" ~ 0,
    Si.alguien.violó.principios.de.pureza.y.decencia. == "No muy relevante" ~ 1,
    Si.alguien.violó.principios.de.pureza.y.decencia. == "Ligeramente relevante" ~ 2,
    Si.alguien.violó.principios.de.pureza.y.decencia. == "Algo relevante" ~ 3,
    Si.alguien.violó.principios.de.pureza.y.decencia. == "Muy relevante" ~ 4,
    Si.alguien.violó.principios.de.pureza.y.decencia. == "Extremadamente relevante" ~ 5,
  ))

# MATH
N156.F01.10 = N156.F01.10 %>%
  mutate(MATH = case_when(
    Si.alguien.era.bueno.en.matemáticas. == "Para nada relevante" ~ 0,
    Si.alguien.era.bueno.en.matemáticas. == "No muy relevante" ~ 1,
    Si.alguien.era.bueno.en.matemáticas. == "Ligeramente relevante" ~ 2,
    Si.alguien.era.bueno.en.matemáticas. == "Algo relevante" ~ 3,
    Si.alguien.era.bueno.en.matemáticas. == "Muy relevante" ~ 4,
    Si.alguien.era.bueno.en.matemáticas. == "Extremadamente relevante" ~ 5,
  ))

# WEAK
N156.F01.10 = N156.F01.10 %>%
  mutate(WEAK = case_when(
    Si.alguien.cuidó.de.alguien.débil.o.vulnerable. == "Para nada relevante" ~ 0,
    Si.alguien.cuidó.de.alguien.débil.o.vulnerable. == "No muy relevante" ~ 1,
    Si.alguien.cuidó.de.alguien.débil.o.vulnerable. == "Ligeramente relevante" ~ 2,
    Si.alguien.cuidó.de.alguien.débil.o.vulnerable. == "Algo relevante" ~ 3,
    Si.alguien.cuidó.de.alguien.débil.o.vulnerable. == "Muy relevante" ~ 4,
    Si.alguien.cuidó.de.alguien.débil.o.vulnerable. == "Extremadamente relevante" ~ 5,
  ))

# UNFAIRLY
N156.F01.10 = N156.F01.10 %>%
  mutate(UNFAIRLY = case_when(
    Si.alguien.actuó.de.forma.injusta. == "Para nada relevante" ~ 0,
    Si.alguien.actuó.de.forma.injusta. == "No muy relevante" ~ 1,
    Si.alguien.actuó.de.forma.injusta. == "Ligeramente relevante" ~ 2,
    Si.alguien.actuó.de.forma.injusta. == "Algo relevante" ~ 3,
    Si.alguien.actuó.de.forma.injusta. == "Muy relevante" ~ 4,
    Si.alguien.actuó.de.forma.injusta. == "Extremadamente relevante" ~ 5,
  ))

# BETRAY
N156.F01.10 = N156.F01.10 %>%
  mutate(BETRAY = case_when(
    Si.alguien.hizo.algo.para.traicionar.a.su.propio.grupo. == "Para nada relevante" ~ 0,
    Si.alguien.hizo.algo.para.traicionar.a.su.propio.grupo. == "No muy relevante" ~ 1,
    Si.alguien.hizo.algo.para.traicionar.a.su.propio.grupo. == "Ligeramente relevante" ~ 2,
    Si.alguien.hizo.algo.para.traicionar.a.su.propio.grupo. == "Algo relevante" ~ 3,
    Si.alguien.hizo.algo.para.traicionar.a.su.propio.grupo. == "Muy relevante" ~ 4,
    Si.alguien.hizo.algo.para.traicionar.a.su.propio.grupo. == "Extremadamente relevante" ~ 5,
  ))

# TRADITIONS
N156.F01.10 = N156.F01.10 %>%
  mutate(TRADITIONS = case_when(
    Si.alguien.actuó.conforme.a.las.tradiciones.de.la.sociedad. == "Para nada relevante" ~ 0,
    Si.alguien.actuó.conforme.a.las.tradiciones.de.la.sociedad. == "No muy relevante" ~ 1,
    Si.alguien.actuó.conforme.a.las.tradiciones.de.la.sociedad. == "Ligeramente relevante" ~ 2,
    Si.alguien.actuó.conforme.a.las.tradiciones.de.la.sociedad. == "Algo relevante" ~ 3,
    Si.alguien.actuó.conforme.a.las.tradiciones.de.la.sociedad. == "Muy relevante" ~ 4,
    Si.alguien.actuó.conforme.a.las.tradiciones.de.la.sociedad. == "Extremadamente relevante" ~ 5,
  ))

# DISGUSTING
N156.F01.10 = N156.F01.10 %>%
  mutate(DISGUSTING = case_when(
    Si.alguien.hizo.algo.asqueroso. == "Para nada relevante" ~ 0,
    Si.alguien.hizo.algo.asqueroso. == "No muy relevante" ~ 1,
    Si.alguien.hizo.algo.asqueroso. == "Ligeramente relevante" ~ 2,
    Si.alguien.hizo.algo.asqueroso. == "Algo relevante" ~ 3,
    Si.alguien.hizo.algo.asqueroso. == "Muy relevante" ~ 4,
    Si.alguien.hizo.algo.asqueroso. == "Extremadamente relevante" ~ 5,
  ))

# CRUEL
N156.F01.10 = N156.F01.10 %>%
  mutate(CRUEL = case_when(
    Si.alguien.fue.cruel. == "Para nada relevante" ~ 0,
    Si.alguien.fue.cruel. == "No muy relevante" ~ 1,
    Si.alguien.fue.cruel. == "Ligeramente relevante" ~ 2,
    Si.alguien.fue.cruel. == "Algo relevante" ~ 3,
    Si.alguien.fue.cruel. == "Muy relevante" ~ 4,
    Si.alguien.fue.cruel. == "Extremadamente relevante" ~ 5,
  ))

# RIGHTS
N156.F01.10 = N156.F01.10 %>%
  mutate(RIGHTS = case_when(
    Si.a.alguien.se.le.negaron.sus.derechos. == "Para nada relevante" ~ 0,
    Si.a.alguien.se.le.negaron.sus.derechos. == "No muy relevante" ~ 1,
    Si.a.alguien.se.le.negaron.sus.derechos. == "Ligeramente relevante" ~ 2,
    Si.a.alguien.se.le.negaron.sus.derechos. == "Algo relevante" ~ 3,
    Si.a.alguien.se.le.negaron.sus.derechos. == "Muy relevante" ~ 4,
    Si.a.alguien.se.le.negaron.sus.derechos. == "Extremadamente relevante" ~ 5,
  ))

# LOYALTY
N156.F01.10 = N156.F01.10 %>%
  mutate(LOYALTY = case_when(
    Si.alguien.mostró.una.falta.de.lealtad. == "Para nada relevante" ~ 0,
    Si.alguien.mostró.una.falta.de.lealtad. == "No muy relevante" ~ 1,
    Si.alguien.mostró.una.falta.de.lealtad. == "Ligeramente relevante" ~ 2,
    Si.alguien.mostró.una.falta.de.lealtad. == "Algo relevante" ~ 3,
    Si.alguien.mostró.una.falta.de.lealtad. == "Muy relevante" ~ 4,
    Si.alguien.mostró.una.falta.de.lealtad. == "Extremadamente relevante" ~ 5,
  ))

# CHAOS
N156.F01.10 = N156.F01.10 %>%
  mutate(CHAOS = case_when(
    Si.una.acción.causó.caos.o.desorden. == "Para nada relevante" ~ 0,
    Si.una.acción.causó.caos.o.desorden. == "No muy relevante" ~ 1,
    Si.una.acción.causó.caos.o.desorden. == "Ligeramente relevante" ~ 2,
    Si.una.acción.causó.caos.o.desorden. == "Algo relevante" ~ 3,
    Si.una.acción.causó.caos.o.desorden. == "Muy relevante" ~ 4,
    Si.una.acción.causó.caos.o.desorden. == "Extremadamente relevante" ~ 5,
  ))

# GOD
N156.F01.10 = N156.F01.10 %>%
  mutate(GOD = case_when(
    Si.alguien.actuó.de.una.manera.que.Dios.aprobaría == "Para nada relevante" ~ 0,
    Si.alguien.actuó.de.una.manera.que.Dios.aprobaría == "No muy relevante" ~ 1,
    Si.alguien.actuó.de.una.manera.que.Dios.aprobaría == "Ligeramente relevante" ~ 2,
    Si.alguien.actuó.de.una.manera.que.Dios.aprobaría == "Algo relevante" ~ 3,
    Si.alguien.actuó.de.una.manera.que.Dios.aprobaría == "Muy relevante" ~ 4,
    Si.alguien.actuó.de.una.manera.que.Dios.aprobaría == "Extremadamente relevante" ~ 5,
  ))

########################################################################################## 2da parte
# COMPASSION
N156.F01.10 = N156.F01.10 %>%
  mutate(COMPASSION = case_when(
    La.compasión.por.los.que.sufren.es.la.virtud.más.importante. == "Totalmente en desacuerdo" ~ 0,
    La.compasión.por.los.que.sufren.es.la.virtud.más.importante. == "Bastante en desacuerdo" ~ 1,
    La.compasión.por.los.que.sufren.es.la.virtud.más.importante. == "Ligeramente en desacuerdo" ~ 2,
    La.compasión.por.los.que.sufren.es.la.virtud.más.importante. == "Ligeramente de acuerdo" ~ 3,
    La.compasión.por.los.que.sufren.es.la.virtud.más.importante. == "Bastante de acuerdo" ~ 4,
    La.compasión.por.los.que.sufren.es.la.virtud.más.importante. == "Totalmente de acuerdo" ~ 5,
  ))

# FAIRLY
N156.F01.10 = N156.F01.10 %>%
  mutate(FAIRLY = case_when(
    Cuando.el.gobierno.hace.leyes..el.principio.número.uno.debe.ser.garantizar.que.todo.el.mundo.reciba.un.trato.justo. == "Totalmente en desacuerdo" ~ 0,
    Cuando.el.gobierno.hace.leyes..el.principio.número.uno.debe.ser.garantizar.que.todo.el.mundo.reciba.un.trato.justo. == "Bastante en desacuerdo" ~ 1,
    Cuando.el.gobierno.hace.leyes..el.principio.número.uno.debe.ser.garantizar.que.todo.el.mundo.reciba.un.trato.justo. == "Ligeramente en desacuerdo" ~ 2,
    Cuando.el.gobierno.hace.leyes..el.principio.número.uno.debe.ser.garantizar.que.todo.el.mundo.reciba.un.trato.justo. == "Ligeramente de acuerdo" ~ 3,
    Cuando.el.gobierno.hace.leyes..el.principio.número.uno.debe.ser.garantizar.que.todo.el.mundo.reciba.un.trato.justo. == "Bastante de acuerdo" ~ 4,
    Cuando.el.gobierno.hace.leyes..el.principio.número.uno.debe.ser.garantizar.que.todo.el.mundo.reciba.un.trato.justo. == "Totalmente de acuerdo" ~ 5,
  ))

# HISTORY
N156.F01.10 = N156.F01.10 %>%
  mutate(HISTORY = case_when(
    Estoy.orgulloso.de.la.historia.de.mi.país. == "Totalmente en desacuerdo" ~ 0,
    Estoy.orgulloso.de.la.historia.de.mi.país. == "Bastante en desacuerdo" ~ 1,
    Estoy.orgulloso.de.la.historia.de.mi.país. == "Ligeramente en desacuerdo" ~ 2,
    Estoy.orgulloso.de.la.historia.de.mi.país. == "Ligeramente de acuerdo" ~ 3,
    Estoy.orgulloso.de.la.historia.de.mi.país. == "Bastante de acuerdo" ~ 4,
    Estoy.orgulloso.de.la.historia.de.mi.país. == "Totalmente de acuerdo" ~ 5,
  ))

# KIDRESPECT
N156.F01.10 = N156.F01.10 %>%
  mutate(KIDRESPECT = case_when(
    El.respeto.a.la.autoridad.es.algo.que.todos.los.niños.deben.aprender. == "Totalmente en desacuerdo" ~ 0,
    El.respeto.a.la.autoridad.es.algo.que.todos.los.niños.deben.aprender. == "Bastante en desacuerdo" ~ 1,
    El.respeto.a.la.autoridad.es.algo.que.todos.los.niños.deben.aprender. == "Ligeramente en desacuerdo" ~ 2,
    El.respeto.a.la.autoridad.es.algo.que.todos.los.niños.deben.aprender. == "Ligeramente de acuerdo" ~ 3,
    El.respeto.a.la.autoridad.es.algo.que.todos.los.niños.deben.aprender. == "Bastante de acuerdo" ~ 4,
    El.respeto.a.la.autoridad.es.algo.que.todos.los.niños.deben.aprender. == "Totalmente de acuerdo" ~ 5,
  ))

# HARMLESSDG
N156.F01.10 = N156.F01.10 %>%
  mutate(HARMLESSDG = case_when(
    La.gente.no.debería.hacer.cosas.que.son.repugnantes..incluso.si.no.hacen.daño.a.nadie. == "Totalmente en desacuerdo" ~ 0,
    La.gente.no.debería.hacer.cosas.que.son.repugnantes..incluso.si.no.hacen.daño.a.nadie. == "Bastante en desacuerdo" ~ 1,
    La.gente.no.debería.hacer.cosas.que.son.repugnantes..incluso.si.no.hacen.daño.a.nadie. == "Ligeramente en desacuerdo" ~ 2,
    La.gente.no.debería.hacer.cosas.que.son.repugnantes..incluso.si.no.hacen.daño.a.nadie. == "Ligeramente de acuerdo" ~ 3,
    La.gente.no.debería.hacer.cosas.que.son.repugnantes..incluso.si.no.hacen.daño.a.nadie. == "Bastante de acuerdo" ~ 4,
    La.gente.no.debería.hacer.cosas.que.son.repugnantes..incluso.si.no.hacen.daño.a.nadie. == "Totalmente de acuerdo" ~ 5,
  ))

# GOOD
N156.F01.10 = N156.F01.10 %>%
  mutate(GOOD = case_when(
    Es.mejor.hacer.el.bien.que.hacer.el.mal. == "Totalmente en desacuerdo" ~ 0,
    Es.mejor.hacer.el.bien.que.hacer.el.mal. == "Bastante en desacuerdo" ~ 1,
    Es.mejor.hacer.el.bien.que.hacer.el.mal. == "Ligeramente en desacuerdo" ~ 2,
    Es.mejor.hacer.el.bien.que.hacer.el.mal. == "Ligeramente de acuerdo" ~ 3,
    Es.mejor.hacer.el.bien.que.hacer.el.mal. == "Bastante de acuerdo" ~ 4,
    Es.mejor.hacer.el.bien.que.hacer.el.mal. == "Totalmente de acuerdo" ~ 5,
  ))

# ANIMAL
N156.F01.10 = N156.F01.10 %>%
  mutate(ANIMAL = case_when(
    Una.de.las.peores.cosas.que.puede.hacer.una.persona.es.herir.a.un.animal.indefenso. == "Totalmente en desacuerdo" ~ 0,
    Una.de.las.peores.cosas.que.puede.hacer.una.persona.es.herir.a.un.animal.indefenso. == "Bastante en desacuerdo" ~ 1,
    Una.de.las.peores.cosas.que.puede.hacer.una.persona.es.herir.a.un.animal.indefenso. == "Ligeramente en desacuerdo" ~ 2,
    Una.de.las.peores.cosas.que.puede.hacer.una.persona.es.herir.a.un.animal.indefenso. == "Ligeramente de acuerdo" ~ 3,
    Una.de.las.peores.cosas.que.puede.hacer.una.persona.es.herir.a.un.animal.indefenso. == "Bastante de acuerdo" ~ 4,
    Una.de.las.peores.cosas.que.puede.hacer.una.persona.es.herir.a.un.animal.indefenso. == "Totalmente de acuerdo" ~ 5,
  ))

# JUSTICE
N156.F01.10 = N156.F01.10 %>%
  mutate(JUSTICE = case_when(
    La.justicia.es.el.requisito.más.importante.para.una.sociedad. == "Totalmente en desacuerdo" ~ 0,
    La.justicia.es.el.requisito.más.importante.para.una.sociedad. == "Bastante en desacuerdo" ~ 1,
    La.justicia.es.el.requisito.más.importante.para.una.sociedad. == "Ligeramente en desacuerdo" ~ 2,
    La.justicia.es.el.requisito.más.importante.para.una.sociedad. == "Ligeramente de acuerdo" ~ 3,
    La.justicia.es.el.requisito.más.importante.para.una.sociedad. == "Bastante de acuerdo" ~ 4,
    La.justicia.es.el.requisito.más.importante.para.una.sociedad. == "Totalmente de acuerdo" ~ 5,
  ))

# FAMILY
N156.F01.10 = N156.F01.10 %>%
  mutate(FAMILY = case_when(
    La.gente.debe.ser.leal.a.los.miembros.de.su.familia..incluso.cuando.estos.han.hecho.algo.malo... == "Totalmente en desacuerdo" ~ 0,
    La.gente.debe.ser.leal.a.los.miembros.de.su.familia..incluso.cuando.estos.han.hecho.algo.malo... == "Bastante en desacuerdo" ~ 1,
    La.gente.debe.ser.leal.a.los.miembros.de.su.familia..incluso.cuando.estos.han.hecho.algo.malo... == "Ligeramente en desacuerdo" ~ 2,
    La.gente.debe.ser.leal.a.los.miembros.de.su.familia..incluso.cuando.estos.han.hecho.algo.malo... == "Ligeramente de acuerdo" ~ 3,
    La.gente.debe.ser.leal.a.los.miembros.de.su.familia..incluso.cuando.estos.han.hecho.algo.malo... == "Bastante de acuerdo" ~ 4,
    La.gente.debe.ser.leal.a.los.miembros.de.su.familia..incluso.cuando.estos.han.hecho.algo.malo... == "Totalmente de acuerdo" ~ 5,
  ))

# SEXROLES
N156.F01.10 = N156.F01.10 %>%
  mutate(SEXROLES = case_when(
    Los.hombres.y.las.mujeres.tienen.roles.diferentes.a.desempeñar.en.la.sociedad. == "Totalmente en desacuerdo" ~ 0,
    Los.hombres.y.las.mujeres.tienen.roles.diferentes.a.desempeñar.en.la.sociedad. == "Bastante en desacuerdo" ~ 1,
    Los.hombres.y.las.mujeres.tienen.roles.diferentes.a.desempeñar.en.la.sociedad. == "Ligeramente en desacuerdo" ~ 2,
    Los.hombres.y.las.mujeres.tienen.roles.diferentes.a.desempeñar.en.la.sociedad. == "Ligeramente de acuerdo" ~ 3,
    Los.hombres.y.las.mujeres.tienen.roles.diferentes.a.desempeñar.en.la.sociedad. == "Bastante de acuerdo" ~ 4,
    Los.hombres.y.las.mujeres.tienen.roles.diferentes.a.desempeñar.en.la.sociedad. == "Totalmente de acuerdo" ~ 5,
  ))

# UNNATURAL
N156.F01.10 = N156.F01.10 %>%
  mutate(UNNATURAL = case_when(
    Yo.calificaría.de.incorrectos.algunos.actos.por.el.hecho.de.ser.antinaturales. == "Totalmente en desacuerdo" ~ 0,
    Yo.calificaría.de.incorrectos.algunos.actos.por.el.hecho.de.ser.antinaturales. == "Bastante en desacuerdo" ~ 1,
    Yo.calificaría.de.incorrectos.algunos.actos.por.el.hecho.de.ser.antinaturales. == "Ligeramente en desacuerdo" ~ 2,
    Yo.calificaría.de.incorrectos.algunos.actos.por.el.hecho.de.ser.antinaturales. == "Ligeramente de acuerdo" ~ 3,
    Yo.calificaría.de.incorrectos.algunos.actos.por.el.hecho.de.ser.antinaturales. == "Bastante de acuerdo" ~ 4,
    Yo.calificaría.de.incorrectos.algunos.actos.por.el.hecho.de.ser.antinaturales. == "Totalmente de acuerdo" ~ 5,
  ))

# KILL
N156.F01.10 = N156.F01.10 %>%
  mutate(KILL = case_when(
    Nunca.puede.ser.correcto.matar.a.un.ser.humano. == "Totalmente en desacuerdo" ~ 0,
    Nunca.puede.ser.correcto.matar.a.un.ser.humano. == "Bastante en desacuerdo" ~ 1,
    Nunca.puede.ser.correcto.matar.a.un.ser.humano. == "Ligeramente en desacuerdo" ~ 2,
    Nunca.puede.ser.correcto.matar.a.un.ser.humano. == "Ligeramente de acuerdo" ~ 3,
    Nunca.puede.ser.correcto.matar.a.un.ser.humano. == "Bastante de acuerdo" ~ 4,
    Nunca.puede.ser.correcto.matar.a.un.ser.humano. == "Totalmente de acuerdo" ~ 5,
  ))

# RICH
N156.F01.10 = N156.F01.10 %>%
  mutate(RICH = case_when(
    Creo.que.es.moralmente.incorrecto.que.los.niños.ricos.hereden.mucho.dinero.mientras.los.niños.pobres.no.heredan.nada. == "Totalmente en desacuerdo" ~ 0,
    Creo.que.es.moralmente.incorrecto.que.los.niños.ricos.hereden.mucho.dinero.mientras.los.niños.pobres.no.heredan.nada. == "Bastante en desacuerdo" ~ 1,
    Creo.que.es.moralmente.incorrecto.que.los.niños.ricos.hereden.mucho.dinero.mientras.los.niños.pobres.no.heredan.nada. == "Ligeramente en desacuerdo" ~ 2,
    Creo.que.es.moralmente.incorrecto.que.los.niños.ricos.hereden.mucho.dinero.mientras.los.niños.pobres.no.heredan.nada. == "Ligeramente de acuerdo" ~ 3,
    Creo.que.es.moralmente.incorrecto.que.los.niños.ricos.hereden.mucho.dinero.mientras.los.niños.pobres.no.heredan.nada. == "Bastante de acuerdo" ~ 4,
    Creo.que.es.moralmente.incorrecto.que.los.niños.ricos.hereden.mucho.dinero.mientras.los.niños.pobres.no.heredan.nada. == "Totalmente de acuerdo" ~ 5,
  ))

# TEAM
N156.F01.10 = N156.F01.10 %>%
  mutate(TEAM = case_when(
    Es.más.importante.trabajar.en.equipo.que.expresarse.uno.mismo. == "Totalmente en desacuerdo" ~ 0,
    Es.más.importante.trabajar.en.equipo.que.expresarse.uno.mismo. == "Bastante en desacuerdo" ~ 1,
    Es.más.importante.trabajar.en.equipo.que.expresarse.uno.mismo. == "Ligeramente en desacuerdo" ~ 2,
    Es.más.importante.trabajar.en.equipo.que.expresarse.uno.mismo. == "Ligeramente de acuerdo" ~ 3,
    Es.más.importante.trabajar.en.equipo.que.expresarse.uno.mismo. == "Bastante de acuerdo" ~ 4,
    Es.más.importante.trabajar.en.equipo.que.expresarse.uno.mismo. == "Totalmente de acuerdo" ~ 5,
  ))

# SOLDIER
N156.F01.10 = N156.F01.10 %>%
  mutate(SOLDIER = case_when(
    Si.yo.fuera.un.soldado.y.no.estuviera.de.acuerdo.con.las.órdenes.de.mi.comandante..obedecería.de.todos.modos.porque.es.mi.deber. == "Totalmente en desacuerdo" ~ 0,
    Si.yo.fuera.un.soldado.y.no.estuviera.de.acuerdo.con.las.órdenes.de.mi.comandante..obedecería.de.todos.modos.porque.es.mi.deber. == "Bastante en desacuerdo" ~ 1,
    Si.yo.fuera.un.soldado.y.no.estuviera.de.acuerdo.con.las.órdenes.de.mi.comandante..obedecería.de.todos.modos.porque.es.mi.deber. == "Ligeramente en desacuerdo" ~ 2,
    Si.yo.fuera.un.soldado.y.no.estuviera.de.acuerdo.con.las.órdenes.de.mi.comandante..obedecería.de.todos.modos.porque.es.mi.deber. == "Ligeramente de acuerdo" ~ 3,
    Si.yo.fuera.un.soldado.y.no.estuviera.de.acuerdo.con.las.órdenes.de.mi.comandante..obedecería.de.todos.modos.porque.es.mi.deber. == "Bastante de acuerdo" ~ 4,
    Si.yo.fuera.un.soldado.y.no.estuviera.de.acuerdo.con.las.órdenes.de.mi.comandante..obedecería.de.todos.modos.porque.es.mi.deber. == "Totalmente de acuerdo" ~ 5,
  ))

# CHASTITY
N156.F01.10 = N156.F01.10 %>%
  mutate(CHASTITY = case_when(
    La.castidad.es.una.virtud.importante.y.valiosa. == "Totalmente en desacuerdo" ~ 0,
    La.castidad.es.una.virtud.importante.y.valiosa. == "Bastante en desacuerdo" ~ 1,
    La.castidad.es.una.virtud.importante.y.valiosa. == "Ligeramente en desacuerdo" ~ 2,
    La.castidad.es.una.virtud.importante.y.valiosa. == "Ligeramente de acuerdo" ~ 3,
    La.castidad.es.una.virtud.importante.y.valiosa. == "Bastante de acuerdo" ~ 4,
    La.castidad.es.una.virtud.importante.y.valiosa. == "Totalmente de acuerdo" ~ 5,
  ))

############################################################################## LIMPIEZA DE BASE OBLIGATORIA
############ FILTRO CAZA-BOBOS
# Filtro en base a las MATH y GOOD
N156.F01.10_clean = subset(N156.F01.10, MATH < 3)
N156.F01.10_clean = subset(N156.F01.10_clean, GOOD > 2)

############ FILTRO SOLO BUENOS AIRES (PROVINCIA Y CIUDAD) => MFQ30
# Creo listado de respuestas para el filtro
L.de.N = c("Buenos Aires", "Ciudad Autónoma de Buenos Aires")

# Filtro para quedarme solo con los Buenos Aires (Provincia y Ciudad)
N156.F01.10_cleanxBSAS = N156.F01.10_clean %>%
  filter(Lugar.de.nacimiento. %in% L.de.N)
##########################################################################################################

# Creo un nuevo dataframe
MFQ30 = data.frame(
  ID = rep(NA, 110))

# Copio los valores de un dataframe a otro
MFQ30$ID = N156.F01.10_cleanxBSAS$ID
# 1er parte
MFQ30$EMOTIONALLY = N156.F01.10_cleanxBSAS$EMOTIONALLY
MFQ30$TREATED = N156.F01.10_cleanxBSAS$TREATED
MFQ30$LOVECOUNTRY = N156.F01.10_cleanxBSAS$LOVECOUNTRY
MFQ30$RESPECT = N156.F01.10_cleanxBSAS$RESPECT
MFQ30$DECENCY = N156.F01.10_cleanxBSAS$DECENCY
MFQ30$MATH = N156.F01.10_cleanxBSAS$MATH
MFQ30$WEAK = N156.F01.10_cleanxBSAS$WEAK
MFQ30$UNFAIRLY = N156.F01.10_cleanxBSAS$UNFAIRLY
MFQ30$BETRAY = N156.F01.10_cleanxBSAS$BETRAY
MFQ30$TRADITIONS = N156.F01.10_cleanxBSAS$TRADITIONS
MFQ30$DISGUSTING = N156.F01.10_cleanxBSAS$DISGUSTING
MFQ30$CRUEL = N156.F01.10_cleanxBSAS$CRUEL
MFQ30$RIGHTS = N156.F01.10_cleanxBSAS$RIGHTS
MFQ30$LOYALTY = N156.F01.10_cleanxBSAS$LOYALTY
MFQ30$CHAOS = N156.F01.10_cleanxBSAS$CHAOS
MFQ30$GOD = N156.F01.10_cleanxBSAS$GOD
# 2da parte
MFQ30$COMPASSION = N156.F01.10_cleanxBSAS$COMPASSION
MFQ30$FAIRLY = N156.F01.10_cleanxBSAS$FAIRLY
MFQ30$HISTORY = N156.F01.10_cleanxBSAS$HISTORY
MFQ30$KIDRESPECT = N156.F01.10_cleanxBSAS$KIDRESPECT
MFQ30$HARMLESSDG = N156.F01.10_cleanxBSAS$HARMLESSDG
MFQ30$GOOD = N156.F01.10_cleanxBSAS$GOOD
MFQ30$ANIMAL = N156.F01.10_cleanxBSAS$ANIMAL
MFQ30$JUSTICE = N156.F01.10_cleanxBSAS$JUSTICE
MFQ30$FAMILY = N156.F01.10_cleanxBSAS$FAMILY
MFQ30$UNNATURAL = N156.F01.10_cleanxBSAS$UNNATURAL
MFQ30$KILL = N156.F01.10_cleanxBSAS$KILL
MFQ30$RICH = N156.F01.10_cleanxBSAS$RICH
MFQ30$TEAM = N156.F01.10_cleanxBSAS$TEAM
MFQ30$SOLDIER = N156.F01.10_cleanxBSAS$SOLDIER
MFQ30$CHASTITY = N156.F01.10_cleanxBSAS$CHASTITY
MFQ30$SEXROLES = N156.F01.10_cleanxBSAS$SEXROLES
# 3er parte
MFQ30$EDAD = N156.F01.10_cleanxBSAS$Edad.
MFQ30$GENERO = N156.F01.10_cleanxBSAS$Género.
MFQ30$LUGAR = N156.F01.10_cleanxBSAS$Lugar.de.nacimiento.
MFQ30$PARTIDO = N156.F01.10_cleanxBSAS$Actual.partido.político.de.preferencia.
MFQ30$ECONOMICO = N156.F01.10_cleanxBSAS$En.esta.escala.del.espectro.político.económico...dónde.te.ubicarías.
MFQ30$CULTURAL = N156.F01.10_cleanxBSAS$En.esta.escala.del.espectro.político.cultural...dónde.te.ubicarías.

# Convierto las variables ECONÓMICO y CULTURAL en categóricas
# ECONOMICO
MFQ30 = MFQ30 %>%
  mutate(ECONOMICO = case_when(
    ECONOMICO == 1 ~ "Izquierda",
    ECONOMICO == 2 ~ "Centro-Izquierda",
    ECONOMICO == 3 ~ "Centro",
    ECONOMICO == 4 ~ "Centro-Derecha",
    ECONOMICO == 5 ~ "Derecha",
  ))

# CULTURAL
MFQ30 = MFQ30 %>%
  mutate(CULTURAL = case_when(
    CULTURAL == 1 ~ "Liberal",
    CULTURAL == 2 ~ "Medio-Liberal",
    CULTURAL == 3 ~ "Medio",
    CULTURAL == 4 ~ "Medio-Conservador",
    CULTURAL == 5 ~ "Conservador",
  ))

# Calculo los resultados para cada MORAL FOUNDATION
# HARM
MFQ30 = MFQ30 %>%
  mutate(MFQ_HARM_AVG = rowMeans(select(., EMOTIONALLY, WEAK, CRUEL, ANIMAL, KILL, COMPASSION)))

# FAIRNESS
MFQ30 = MFQ30 %>%
  mutate(MFQ_FAIRNESS_AVG = rowMeans(select(., RIGHTS, UNFAIRLY, TREATED, JUSTICE, FAIRLY, RICH)))

# INGROUP
MFQ30 = MFQ30 %>%
  mutate(MFQ_INGROUP_AVG = rowMeans(select(., LOYALTY, BETRAY, LOVECOUNTRY, TEAM, HISTORY, FAMILY)))

# AUTHORITY
MFQ30 = MFQ30 %>%
  mutate(MFQ_AUTHORITY_AVG = rowMeans(select(., TRADITIONS, RESPECT, CHAOS, SEXROLES, SOLDIER, KIDRESPECT)))

# PURITY
MFQ30 = MFQ30 %>%
  mutate(MFQ_PURITY_AVG = rowMeans(select(., DISGUSTING, DECENCY, GOD, HARMLESSDG, UNNATURAL, CHASTITY)))

############################################################################## ANÁLISIS ESTADÍSTICO
### Hago un density plot de cada variable para chequear si tienen una distribución normal
# Las pongo todas juntas en un mismo panel
#install.packages("patchwork")
library(patchwork)
library(ggplot2)

HARM_density + 
  FAIRNESS_density + 
  INGROUP_density + 
  AUTHORITY_density + 
  PURITY_density +
  plot_annotation(tag_levels = "A") 

##################################### Análisis de variables
"""
ECONÓMICO agrupado x cada FUNDAMENTO MORAL
ANOVA + POST-HOC (Tukey)
"""

# Agrupo en izquierda, centro y derecha
# CULTURAL
MFQ30 = MFQ30 %>%
  mutate(ECONOMICO_agrupado = case_when(
    ECONOMICO == "Izquierda" ~ "IZQUIERDA",
    ECONOMICO == "Centro-Izquierda" ~ "IZQUIERDA",
    ECONOMICO == "Centro" ~ "CENTRO",
    ECONOMICO == "Centro-Derecha" ~ "DERECHA",
    ECONOMICO == "Derecha" ~ "DERECHA",
  ))

# Proporciones
tabla4 = table(MFQ30$ECONOMICO_agrupado)
ECONOMICO_agrupado_prop = prop.table(tabla4)
print(ECONOMICO_agrupado_prop * 100)

# Cantidad
resultados4 = MFQ30 %>%
  count(ECONOMICO_agrupado)

#########################################################################################################
#########################################################################################################
#########################################################################################################
##### MORAL DE LA IZQUIERDA
# Corroboro homogeneidad de la varianza dentro de los grupos => NO CUMPLE (resultado significativo)
MORAL_IZQ_levene = leveneTest(Media ~ Moral, data = filter(MFQ30_long, ECONOMICO_agrupado == "IZQUIERDA"))
print(MORAL_IZQ_levene)

# Corro el Kruskal-Wallis Test
MORAL_IZQ = kruskal.test(Media ~ Moral, data = filter(MFQ30_long, ECONOMICO_agrupado == "IZQUIERDA"))
print(MORAL_IZQ)
# Significativo

# Post-Hoc
library(FSA)
MORAL_IZQ_PH = dunnTest(Media ~ Moral, data = filter(MFQ30_long, ECONOMICO_agrupado == "IZQUIERDA"), method = "bonferroni")
print(MORAL_IZQ_PH)

### Gráfico
# Ploteo
ggplot(data = filter(MFQ30_long, ECONOMICO_agrupado == "IZQUIERDA"), 
       aes(x = Moral,
           y = Media, fill = Moral, color = Moral, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("Fundamentos morales en la IZQUIERDA") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  scale_x_discrete(labels = c("Authority", "Fairness", "Harm", "Ingroup", "Purity")) +
  ylim(0,5)

##### MORAL DE LA DERECHA
# Corroboro homogeneidad de la varianza dentro de los grupos => NO CUMPLE (resultado significativo)
MORAL_DER_levene = leveneTest(Media ~ Moral, data = filter(MFQ30_long, ECONOMICO_agrupado == "DERECHA"))
print(MORAL_IZQ_levene)

# Corro el Kruskal-Wallis Test
MORAL_DER = kruskal.test(Media ~ Moral, data = filter(MFQ30_long, ECONOMICO_agrupado == "DERECHA"))
print(MORAL_DER)
# Significativo

# Post-Hoc
library(FSA)
MORAL_DER_PH = dunnTest(Media ~ Moral, data = filter(MFQ30_long, ECONOMICO_agrupado == "DERECHA"), method = "bonferroni")
print(MORAL_DER_PH)

### Gráfico
# Ploteo
ggplot(data = filter(MFQ30_long, ECONOMICO_agrupado == "DERECHA"), 
       aes(x = Moral,
           y = Media, fill = Moral, color = Moral, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("Fundamentos morales en la IZQUIERDA") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  scale_x_discrete(labels = c("Authority", "Fairness", "Harm", "Ingroup", "Purity")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

#########################################################################################################
#########################################################################################################
#########################################################################################################

###### HARM

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(Media = mean(MFQ_HARM_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(DesvioEstandar = sd(MFQ_HARM_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

### Análisis estadístico
# Corroboro distribución normal de la variable dependiente => CUMPLE
HARM_density = MFQ30 %>% 
  ggplot(aes(x = MFQ_HARM_AVG)) +
  geom_density(fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(MFQ30$MFQ_HARM_AVG, na.rm = TRUE), 
                            sd = sd(MFQ30$MFQ_HARM_AVG, na.rm = TRUE)), 
                color = "magenta", 
                alpha = 0.2, 
                geom = "area") +
  ylab("Density") +
  xlab("HARM")

# Corroboro homogeneidad de la varianza dentro de los grupos => NO CUMPLE (resultado significativo)
library(car)
HARMxECO_levene = leveneTest(MFQ_HARM_AVG ~ ECONOMICO_agrupado, data = MFQ30)
print(HARMxECO_levene)

# Corro el Kruskal-Wallis Test
HARMxECOagrup = kruskal.test(MFQ_HARM_AVG ~ ECONOMICO_agrupado, data = MFQ30)
print(HARMxECOagrup)
# Significativo

# Post-Hoc
HARMxECOagrup_PH = dunnTest(MFQ_HARM_AVG ~ ECONOMICO_agrupado, data = MFQ30, method = "bonferroni")
print(HARMxECOagrup_PH)
# DERECHA - IZQUIERDA

### Gráfico
# Creo una variable para marcar el orden en que quiero que aparezcan los valores en el eje X
orden1 = c("IZQUIERDA", "CENTRO", "DERECHA")

# Chequeo las fuentes disponibles
fuentes = windowsFonts()

# Ploteo
MFQ30 %>%
  ggplot(aes(x = factor(ECONOMICO_agrupado, levels = orden1), 
             y = MFQ_HARM_AVG, fill = ECONOMICO_agrupado, color = ECONOMICO_agrupado, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  scale_fill_manual(values = c("#3F2E50","#3E82A9","#C13B48")) +
  scale_color_manual(values = c("black", "#306482", "#8D2A34")) +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("DAÑO/CUIDADO") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### FAIRNESS

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(Media = mean(MFQ_FAIRNESS_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(DesvioEstandar = sd(MFQ_FAIRNESS_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

### Análisis estadístico
# Corroboro distribución normal de la variable dependiente => CUMPLE
FAIRNESS_density = MFQ30 %>% 
  ggplot(aes(x = MFQ_FAIRNESS_AVG)) +
  geom_density(fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(MFQ30$MFQ_FAIRNESS_AVG, na.rm = TRUE), 
                            sd = sd(MFQ30$MFQ_FAIRNESS_AVG, na.rm = TRUE)), 
                color = "magenta", 
                alpha = 0.2, 
                geom = "area") +
  ylab("Density") +
  xlab("FAIRNESS")

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
FAIRxECO_levene = leveneTest(MFQ_FAIRNESS_AVG ~ ECONOMICO_agrupado, data = MFQ30)
print(FAIRxECO_levene)

# Corro el ANOVA
FAIRxECOagrup = aov(MFQ_FAIRNESS_AVG ~ ECONOMICO_agrupado, data = MFQ30)
summary(FAIRxECOagrup)
# Significativo

# Post-Hoc
FAIRxECOagrup_PH = TukeyHSD(FAIRxECOagrup)
print(FAIRxECOagrup_PH)
# DERECHA-CENTRO
# IZQUIERDA-DERECHA

### Gráfico
# Ploteo
MFQ30 %>%
  ggplot(aes(x = factor(ECONOMICO_agrupado, levels = orden1), 
             y = MFQ_FAIRNESS_AVG, fill = ECONOMICO_agrupado, color = ECONOMICO_agrupado, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  scale_fill_manual(values = c("#3F2E50","#3E82A9","#C13B48")) +
  scale_color_manual(values = c("black", "#306482", "#8D2A34")) +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("EQUIDAD/RECIPROCIDAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### INGROUP

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(Media = mean(MFQ_INGROUP_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(DesvioEstandar = sd(MFQ_INGROUP_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

### Análisis estadístico
# Corroboro distribución normal de la variable dependiente => CUMPLE
INGROUP_density = MFQ30 %>% 
  ggplot(aes(x = MFQ_INGROUP_AVG)) +
  geom_density(fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(MFQ30$MFQ_INGROUP_AVG, na.rm = TRUE), 
                            sd = sd(MFQ30$MFQ_INGROUP_AVG, na.rm = TRUE)), 
                color = "magenta", 
                alpha = 0.2, 
                geom = "area") +
  ylab("Density") +
  xlab("INGROUP")

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
INGROUPxECO_levene = leveneTest(MFQ_INGROUP_AVG ~ ECONOMICO_agrupado, data = MFQ30)
print(INGROUPxECO_levene)

# Corro el ANOVA
INGROUPxECOagrup = aov(MFQ_INGROUP_AVG ~ ECONOMICO_agrupado, data = MFQ30)
summary(INGROUPxECOagrup)
# NO significativo - 0.09

### Gráfico
# Ploteo
MFQ30 %>%
  ggplot(aes(x = factor(ECONOMICO_agrupado, levels = orden1), 
             y = MFQ_INGROUP_AVG, fill = ECONOMICO_agrupado, color = ECONOMICO_agrupado, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  scale_fill_manual(values = c("#3F2E50","#3E82A9","#C13B48")) +
  scale_color_manual(values = c("black", "#306482", "#8D2A34")) +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("GRUPO/LEALTAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### AUTHORITY

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(Media = mean(MFQ_AUTHORITY_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(DesvioEstandar = sd(MFQ_AUTHORITY_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

### Análisis estadístico
# Corroboro distribución normal de la variable dependiente => CUMPLE
AUTHORITY_density = MFQ30 %>% 
  ggplot(aes(x = MFQ_AUTHORITY_AVG)) +
  geom_density(fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(MFQ30$MFQ_AUTHORITY_AVG, na.rm = TRUE), 
                            sd = sd(MFQ30$MFQ_AUTHORITY_AVG, na.rm = TRUE)), 
                color = "magenta", 
                alpha = 0.2, 
                geom = "area") +
  ylab("Density") +
  xlab("AUTHORITY")

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
AUTHORITYxECO_levene = leveneTest(MFQ_AUTHORITY_AVG ~ ECONOMICO_agrupado, data = MFQ30)
print(AUTHORITYxECO_levene)

# Corro el ANOVA
AUTHORITYxECOagrup = aov(MFQ_AUTHORITY_AVG ~ ECONOMICO_agrupado, data = MFQ30)
summary(AUTHORITYxECOagrup)
# Significativo

# Post-Hoc
AUTHORITYxECOagrup_PH = TukeyHSD(AUTHORITYxECOagrup)
print(AUTHORITYxECOagrup_PH)
# DERECHA - IZQUIERDA
# NO SIGNIFICATIVO: Centro - Izquierda (0.057)

### Gráfico
# Ploteo
MFQ30 %>%
  ggplot(aes(x = factor(ECONOMICO_agrupado, levels = orden1), 
             y = MFQ_AUTHORITY_AVG, fill = ECONOMICO_agrupado, color = ECONOMICO_agrupado, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  scale_fill_manual(values = c("#3F2E50","#3E82A9","#C13B48")) +
  scale_color_manual(values = c("black", "#306482", "#8D2A34")) +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("AUTORIDAD/RESPETO") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### PURITY

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(Media = mean(MFQ_PURITY_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(ECONOMICO_agrupado) %>%
  summarize(DesvioEstandar = sd(MFQ_PURITY_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

### Análisis estadístico
# Corroboro distribución normal de la variable dependiente => CUMPLE
PURITY_density = MFQ30 %>% 
  ggplot(aes(x = MFQ_PURITY_AVG)) +
  geom_density(fill = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(MFQ30$MFQ_PURITY_AVG, na.rm = TRUE), 
                            sd = sd(MFQ30$MFQ_PURITY_AVG, na.rm = TRUE)), 
                color = "magenta", 
                alpha = 0.2, 
                geom = "area") +
  ylab("Density") +
  xlab("PURITY")

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
PURITYxECO_levene = leveneTest(MFQ_PURITY_AVG ~ ECONOMICO_agrupado, data = MFQ30)
print(PURITYxECO_levene)

# Corro el ANOVA
PURITYxECOagrup = aov(MFQ_PURITY_AVG ~ ECONOMICO_agrupado, data = MFQ30)
summary(PURITYxECOagrup)
# Significativo

# Post-Hoc
PURITYxECOagrup_PH = TukeyHSD(PURITYxECOagrup)
print(PURITYxECOagrup_PH)
# IZQUIERDA-CENTRO
# IZQUIERDA-DERECHA

### Gráfico
# Ploteo
MFQ30 %>%
  ggplot(aes(x = factor(ECONOMICO_agrupado, levels = orden1), 
             y = MFQ_PURITY_AVG, fill = ECONOMICO_agrupado, color = ECONOMICO_agrupado, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  scale_fill_manual(values = c("#3F2E50","#3E82A9","#C13B48")) +
  scale_color_manual(values = c("black", "#306482", "#8D2A34")) +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("PUREZA/SANTIDAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

"""
PATIDOS TOP 5 x cada FUNDAMENTO MORAL
ANOVA + POST-HOC (Tukey)
"""

### Me quedo con únicamento con los 6 partidos más votados
# Calculo la frecuencia de cada categoría
frecuencia = table(MFQ30$PARTIDO)

# Ordeno la frecuencia de manera descendente y tomo las 5 primeras respuestas
top_6_partidos = head(sort(frecuencia, decreasing = TRUE), 6)

# Extraigo las palabras clave de la tabla
nombres_partidos = rownames(top_6_partidos)

# Filtro para quedarme solo con los 5 principales partidos políticos
MFQ30xT5 = MFQ30 %>%
  filter(PARTIDO %in% nombres_partidos)

# Convierto las respuestas "Voto en blanco" en "No sabe / No contesta" para que sea un solo grupo
MFQ30xT5 = MFQ30xT5 %>%
  mutate(PARTIDO = case_when(
    PARTIDO == "Voto en blanco" ~ "No sabe / No contesta",
    PARTIDO == "No sabe / No contesta" ~ "No sabe / No contesta",
    PARTIDO == "Juntos por el Cambio" ~ "Juntos por el Cambio",
    PARTIDO == "La Libertad Avanza" ~ "La Libertad Avanza",
    PARTIDO == "Unión por la Patria" ~ "Unión por la Patria",
    PARTIDO == "Frente de Izquierda y de Trabajadores - Unidad" ~ "Frente de Izquierda y de Trabajadores - Unidad",
  ))

# Proporción
tabla3 = table(MFQ30xT5$PARTIDO)
PARTIDO_prop = prop.table(tabla3)
print(PARTIDO_prop * 100)

# Cantidad
resultados2 = MFQ30xT5 %>%
  count(PARTIDO)

###### HARM

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
HARMxPARTIDO_levene = leveneTest(MFQ_HARM_AVG ~ PARTIDO, data = MFQ30xT5)
print(HARMxPARTIDO_levene)

# Corro el ANOVA
HARMxPARTIDO = aov(MFQ_HARM_AVG ~ PARTIDO, data = MFQ30xT5)
summary(HARMxPARTIDO)
# NO significativo (0.0513)

### Gráfico
# Creo una variable para marcar el orden en que quiero que aparezcan los valores en el eje X
orden2 = c("Frente de Izquierda y de Trabajadores - Unidad", 
           "Unión por la Patria", 
           "No sabe / No contesta",
           "Juntos por el Cambio",
           "La Libertad Avanza")

# Ploteo
MFQ30xT5 %>%
  ggplot(aes(x = factor(PARTIDO, levels = orden2), 
             y = MFQ_HARM_AVG, fill = PARTIDO, color = PARTIDO, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Media") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  ggtitle("DAÑO/CUIDADO") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  scale_x_discrete(labels = c("FIT", "UP", "ns/nc", "JxC", "LLA")) +
  ylim(0,5)

###### FAIRNESS

# Calculo la media y el desvío estándar
resultados8 = MFQ30xT5 %>%
  group_by(PARTIDO) %>%
  summarize(Media = mean(MFQ_FAIRNESS_AVG, na.rm = TRUE))

resultados9 = MFQ30xT5 %>%
  group_by(PARTIDO) %>%
  summarize(DesvioEstandar = sd(MFQ_FAIRNESS_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
FAIRNESSxPARTIDO_levene = leveneTest(MFQ_FAIRNESS_AVG ~ PARTIDO, data = MFQ30xT5)
print(FAIRNESSxPARTIDO_levene)

# Corro el ANOVA
FAIRNESSxPARTIDO = aov(MFQ_FAIRNESS_AVG ~ PARTIDO, data = MFQ30xT5)
summary(FAIRNESSxPARTIDO)
# Significativo

# Post-Hoc
FAIRNESSxPARTIDO_PH = TukeyHSD(FAIRNESSxPARTIDO)
print(FAIRNESSxPARTIDO_PH)
# La Libertad Avanza-Frente de Izquierda y de Trabajadores - Unidad
# Unión por la Patria-Juntos por el Cambio
# Unión por la Patria-La Libertad Avanza 

### Gráfico
# Ploteo
MFQ30xT5 %>%
  ggplot(aes(x = factor(PARTIDO, levels = orden2), 
             y = MFQ_FAIRNESS_AVG, fill = PARTIDO, color = PARTIDO, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  scale_fill_manual(values = c("#C13B48", "#B0A330","#57B35D", "#3F2E50", "#0EB9ED")) +
  scale_color_manual(values = c("#8D2A34", "#B0A330", "#57B35D", "black", "#0CA5D4")) +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("EQUIDAD/RECIPROCIDAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  scale_x_discrete(labels = c("FIT", "UP", "ns/nc", "JxC", "LLA")) +
  ylim(0,5)

###### INGROUP

# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
INGROUPxPARTIDO_levene = leveneTest(MFQ_INGROUP_AVG ~ PARTIDO, data = MFQ30xT5)
print(INGROUPxPARTIDO_levene)

# Corro el ANOVA
INGROUPxPARTIDO = aov(MFQ_INGROUP_AVG ~ PARTIDO, data = MFQ30xT5)
summary(INGROUPxPARTIDO)
# NO significativo

### Gráfico
# Ploteo
MFQ30xT5 %>%
  ggplot(aes(x = factor(PARTIDO, levels = orden2), 
             y = MFQ_INGROUP_AVG, fill = PARTIDO, color = PARTIDO, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Media") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  ggtitle("GRUPO/LEALTAD") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  scale_x_discrete(labels = c("FIT", "UP", "ns/nc", "JxC", "LLA")) +
  ylim(0,5)

###### AUTHORITY

# Corroboro homogeneidad de la varianza dentro de los grupos => NO CUMPLE (resultado significativo)
AUTHORITYxPARTIDO_levene = leveneTest(MFQ_AUTHORITY_AVG ~ PARTIDO, data = MFQ30xT5)
print(AUTHORITYxPARTIDO_levene)

# Corro el Kruskal-Wallis Test
AUTHORITYxPARTIDO = kruskal.test(MFQ_AUTHORITY_AVG ~ PARTIDO, data = MFQ30xT5)
print(AUTHORITYxPARTIDO)
# Significativo

# Post-Hoc
AUTHORITYxPARTIDO_PH = dunnTest(MFQ_AUTHORITY_AVG ~ PARTIDO, data = MFQ30xT5, method = "bonferroni")
print(AUTHORITYxPARTIDO_PH)
# NO significativo

### Gráfico
# Ploteo
MFQ30xT5 %>%
  ggplot(aes(x = factor(PARTIDO, levels = orden2), 
             y = MFQ_AUTHORITY_AVG, fill = PARTIDO, color = PARTIDO, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Media") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  ggtitle("AUTORIDAD/RESPETO") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  scale_x_discrete(labels = c("FIT", "UP", "ns/nc", "JxC", "LLA")) +
  ylim(0,5)

###### PURITY

# Corroboro homogeneidad de la varianza dentro de los grupos => NO CUMPLE (resultado significativo)
PURITYxPARTIDO_levene = leveneTest(MFQ_PURITY_AVG ~ PARTIDO, data = MFQ30xT5)
print(PURITYxPARTIDO_levene)

# Corro el Kruskal-Wallis Test
PURITYxPARTIDO = kruskal.test(MFQ_PURITY_AVG ~ PARTIDO, data = MFQ30xT5)
print(PURITYxPARTIDO)
# Significativo

# Post-Hoc
PURITYxPARTIDO_PH = dunnTest(MFQ_PURITY_AVG ~ PARTIDO, data = MFQ30xT5, method = "bonferroni")
print(PURITYxPARTIDO_PH)
# No sabe / No contesta - Unión por la Patria

### Gráfico
# Ploteo
MFQ30xT5 %>%
  ggplot(aes(x = factor(PARTIDO, levels = orden2), 
             y = MFQ_PURITY_AVG, fill = PARTIDO, color = PARTIDO, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Media") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  ggtitle("PUREZA/SANTIDAD") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  scale_x_discrete(labels = c("FIT", "UP", "ns/nc", "JxC", "LLA")) +
  ylim(0,5)

"""
CULTURAL x cada FUNDAMENTO MORAL
ANOVA + POST-HOC (Tukey)
"""
# Proporciones
tabla5 = table(MFQ30$CULTURAL)
CULTURAL_prop = prop.table(tabla5)
print(CULTURAL_prop * 100)

# Cantidad
resultados5 = MFQ30 %>%
  count(CULTURAL)

###### HARM
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
HARMxCULTURAL_levene = leveneTest(MFQ_HARM_AVG ~ CULTURAL, data = MFQ30)
print(HARMxCULTURAL_levene)

# Corro el ANOVA
HARMxCULTURAL = aov(MFQ_HARM_AVG ~ CULTURAL, data = MFQ30)
summary(HARMxCULTURAL)
# NO significativo

### Gráfico
# Creo una variable para marcar el orden en que quiero que aparezcan las categorías
orden3 = c("Liberal", 
           "Medio-Liberal", 
           "Medio",
           "Medio-Conservador",
           "Conservador")

# Ploteo
filter(MFQ30, CULTURAL != "NA") %>%
  ggplot(aes(x = MFQ_HARM_AVG, fill = factor(CULTURAL, levels = orden3))) +
  geom_density(alpha = 0.5) +
  labs(x = "Media", y = "Densidad", fill = "Escala político-cultural") +
  theme_bw() +
  ggtitle("DAÑO/CUIDADO") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16, face = "bold")) +
  ylim(0,1) +
  xlim(0,5)

###### FAIRNESS
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
FAIRNESSxCULTURAL_levene = leveneTest(MFQ_FAIRNESS_AVG ~ CULTURAL, data = MFQ30)
print(FAIRNESSxCULTURAL_levene)

# Corro el ANOVA
FAIRNESSxCULTURAL = aov(MFQ_FAIRNESS_AVG ~ CULTURAL, data = MFQ30)
summary(FAIRNESSxCULTURAL)
# NO significativo (0.0503)

### Gráfico
# Ploteo
filter(MFQ30, CULTURAL != "NA") %>%
  ggplot(aes(x = MFQ_FAIRNESS_AVG, fill = factor(CULTURAL, levels = orden3))) +
  geom_density(alpha = 0.5) +
  labs(x = "Media", y = "Densidad", fill = "Escala político-cultural") +
  theme_bw() +
  ggtitle("EQUIDAD/RECIPROCIDAD") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16, face = "bold")) +
  ylim(0,1) +
  xlim(0,5)

###### INGROUP
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
INGROUPxCULTURAL_levene = leveneTest(MFQ_INGROUP_AVG ~ CULTURAL, data = MFQ30)
print(INGROUPxCULTURAL_levene)

# Corro el ANOVA
INGROUPxCULTURAL = aov(MFQ_INGROUP_AVG ~ CULTURAL, data = MFQ30)
summary(INGROUPxCULTURAL)
# Significativo

# Post-Hoc
INGROUPxCULTURAL_PH = TukeyHSD(INGROUPxCULTURAL)
print(INGROUPxCULTURAL_PH)
# NO significativo

### Gráfico
# Ploteo
filter(MFQ30, CULTURAL != "NA") %>%
  ggplot(aes(x = MFQ_INGROUP_AVG, fill = factor(CULTURAL, levels = orden3))) +
  geom_density(alpha = 0.5) +
  labs(x = "Media", y = "Densidad", fill = "Escala político-cultural") +
  theme_bw() +
  ggtitle("GRUPO/LEALTAD") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16, face = "bold")) +
  ylim(0,1) +
  xlim(0,5)

###### AUTHORITY
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
AUTHORITYxCULTURAL_levene = leveneTest(MFQ_AUTHORITY_AVG ~ CULTURAL, data = MFQ30)
print(AUTHORITYxCULTURAL_levene)

# Corro el ANOVA
AUTHORITYxCULTURAL = aov(MFQ_AUTHORITY_AVG ~ CULTURAL, data = MFQ30)
summary(AUTHORITYxCULTURAL)
# NO significativo (0.069)

### Gráfico
# Ploteo
filter(MFQ30, CULTURAL != "NA") %>%
  ggplot(aes(x = MFQ_AUTHORITY_AVG, fill = factor(CULTURAL, levels = orden3))) +
  geom_density(alpha = 0.5) +
  labs(x = "Media", y = "Densidad", fill = "Escala político-cultural") +
  theme_bw() +
  ggtitle("AUTORIDAD/RESPETO") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16, face = "bold")) +
  ylim(0,1) +
  xlim(0,5)

###### PURITY
# Corroboro homogeneidad de la varianza dentro de los grupos => NO CUMPLE (resultado significativo)
PURITYxCULTURAL_levene = leveneTest(MFQ_PURITY_AVG ~ CULTURAL, data = MFQ30)
print(PURITYxCULTURAL_levene)

# Corro el Kruskal-Wallis Test
PURITYxCULTURAL = kruskal.test(MFQ_PURITY_AVG ~ CULTURAL, data = MFQ30)
print(PURITYxCULTURAL)
# Significativo

# Post-Hoc
PURITYxCULTURAL_PH = dunnTest(MFQ_PURITY_AVG ~ CULTURAL, data = MFQ30, method = "bonferroni")
print(PURITYxCULTURAL_PH)
# NO significativo

### Gráfico
# Ploteo
filter(MFQ30, CULTURAL != "NA") %>%
  ggplot(aes(x = MFQ_PURITY_AVG, fill = factor(CULTURAL, levels = orden3))) +
  geom_density(alpha = 0.5) +
  labs(x = "Media", y = "Densidad", fill = "Escala político-cultural") +
  theme_bw() +
  ggtitle("PUREZA/SANTIDAD") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16, face = "bold")) +
  ylim(0,1) +
  xlim(0,5)

############################################################################## ANÁLISIS GENERAL
"""
MUESTRA TOTAL x MORAL
"""

# Combino las columnas en una sola variable llamada "Moral"
MFQ30_long = tidyr::gather(MFQ30, 
                           key = "Moral", 
                           value = "Media", 
                           MFQ_HARM_AVG, 
                           MFQ_FAIRNESS_AVG, 
                           MFQ_INGROUP_AVG, 
                           MFQ_AUTHORITY_AVG, 
                           MFQ_PURITY_AVG)

# Calculo la media y el desvío estándar
resultados6 = MFQ30_long %>%
  group_by(Moral) %>%
  summarize(Media = mean(Media, na.rm = TRUE))

resultados7 = MFQ30_long %>%
  group_by(Moral) %>%
  summarize(DesvioEstandar = sd(Media, na.rm = TRUE))

# Muestro los resultados
print(resultados6)
print(resultados7)

### Gráfico
# Ploteo
MFQ30_long %>% 
  ggplot(aes(x = Moral, y = Media)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "", y = "Puntaje") +
  theme_bw() +
  #ggtitle("Puntaje en cada fundamento moral")
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold", angle = 45, hjust = 1)) +
  ylim(0,5) +
  scale_x_discrete(labels = 
                     c("MFQ_AUTHORITY_AVG" = "Autoridad/Respeto", 
                      "MFQ_FAIRNESS_AVG" = "Equidad/Reciprocidad", 
                      "MFQ_HARM_AVG" = "Daño/Cuidado", 
                      "MFQ_INGROUP_AVG" = "Grupo/Lealtad", 
                      "MFQ_PURITY_AVG" = "Pureza/Santidad"))

"""
MUESTRA TOTAL x ECONÓMICO
"""
# Creo una variable para marcar el orden en que quiero que aparezcan los valores en el eje X
orden4 = c("Izquierda", 
           "Centro-Izquierda", 
           "Centro",
           "Centro-Derecha",
           "Derecha")

### Gráfico
# Ploteo
MFQ30 %>% 
  ggplot(aes(x = factor(ECONOMICO, levels = orden4))) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
  labs(x = "", y = "Porcentaje del total") +
  theme_bw() +
  #ggtitle("Proporción de la muestra en la escala político-económica")
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold"))

# Proporciones
tabla3 = table(MFQ30$ECONOMICO)
ECONOMICO_prop = prop.table(tabla3)
print(ECONOMICO_prop * 100)

# Cantidad
resultados3 = MFQ30 %>%
  count(ECONOMICO)

"""
MUESTRA TOTAL x CULTURAL
"""

### Gráfico
# Ploteo
filter(MFQ30, CULTURAL != "NA") %>% 
  ggplot(aes(x = factor(CULTURAL, levels = orden3))) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
  labs(x = "", y = "Porcentaje") +
  theme_bw() +
  ggtitle("Proporción de la muestra en la escala político-cultural") +
  theme(plot.title = element_text(family = "sans", size = 18, face = "bold"),
        axis.title = element_text(family = "sans", size = 16),
        axis.text = element_text(family = "sans", size = 14))

"""
EDAD x MORAL
"""
###### Agrupo las edades

### OPCIÓN 1
# Baby boomers: 59 a 77
# Generación X: 43 a 58
# Millennials: 27 a 42
# Generación Z: 18 a 26

"""
Baby Boomers: Nacidos entre 1946-1964
Generación X: Nacidos entre 1965-1980
Millennials: Nacidos entre 1981-1996
Generación Z: Nacidos entre 1997-2012
"""

### OPCIÓN 2
# 18 a 25
# 25 a 35
# 35 a 50
# +50

MFQ30 = MFQ30 %>%
  mutate(EDAD_agrupada = case_when(
    EDAD <= 26 ~ "Generación Z",
    EDAD > 26 & EDAD <= 42 ~ "Millennials",
    EDAD > 42 & EDAD <= 58 ~ "Generación X",
    EDAD > 58 ~ "Baby Boomers",
  ))

### Gráfico
# Creo una variable para marcar el orden en que quiero que aparezcan los valores en el eje X
orden5 = c("Generación X", 
           "Millennials",
           "Generación Z")

# Plot
MFQ30 %>% 
  ggplot(aes(x = factor(EDAD_agrupada, levels = orden5))) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
  labs(x = "", y = "Porcentaje") +
  theme_bw() +
  #ggtitle("Proporción de la muestra a nivel generacional")
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
        axis.title = element_text(family = "serif", size = 17),
        axis.text.y = element_text(family = "serif", size = 19),
        axis.text.x = element_text(family = "serif", size = 21, face = "bold"))

# Proporciones
tabla1 = table(MFQ30$EDAD_agrupada)
EDAD_agrupada_prop = prop.table(tabla1)
print(EDAD_agrupada_prop * 100)

# Cantidad
resultados2 = MFQ30 %>%
  count(EDAD_agrupada)

# Calculo la media y el desvío estándar por categoría de 'EDAD_agrupada'
resultados1 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(Media = mean(EDAD, na.rm = TRUE),
            DesvioEstandar = sd(EDAD, na.rm = TRUE),
            Total = count(EDAD_agrupada, na.rm = TRUE))

# Muestro los resultados
print(resultados1)

"""
MUESTRA TOTAL x GÉNERO
"""
MFQ30 %>% 
  ggplot(aes(x = GENERO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
  labs(x = "", y = "Porcentaje") +
  theme_bw() +
  #ggtitle("Proporción de la muestra respecto al género") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold"))

# Proporciones
tabla2 = table(MFQ30$GENERO)
GENERO_prop = prop.table(tabla2)
print(GENERO_prop * 100)

# Cantidad
resultados2 = MFQ30 %>%
  count(GENERO)

"""
MUESTRA TOTAL x GEOGRÁFICO
"""
MFQ30 %>% 
  ggplot(aes(x = LUGAR)) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100)) +
  labs(x = "", y = "Porcentaje") +
  theme_bw() +
  #ggtitle("Proporción de la muestra respecto a la ubicación geográfica") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold"))

# Proporción
tabla3 = table(MFQ30$LUGAR)
LUGAR_prop = prop.table(tabla3)
print(LUGAR_prop * 100)

# Cantidad
resultados2 = MFQ30 %>%
  count(LUGAR)

"""
MUESTRA TOTAL x PARTIDO
"""
MFQ30xT5 %>% 
  ggplot(aes(x = factor(PARTIDO, levels = orden2)), 
         (y = (..count..)/sum(..count..)*100), color = PARTIDO) +
  geom_bar() +
  labs(x = "", y = "Porcentaje") +
  theme_bw() +
  #ggtitle("Proporción de la muestra respecto al partido de preferencia") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold"))+
  scale_x_discrete(labels = c("FIT", "UP", "ns/nc", "JxC", "LLA"))

############################################################################## ANÁLISIS ESTADÍSTICO
###### HARM
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
#library(car)
HARMxEDADagrup_levene = leveneTest(MFQ_HARM_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
print(HARMxEDADagrup_levene)

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(Media = mean(MFQ_HARM_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(DesvioEstandar = sd(MFQ_HARM_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

# Corro el ANOVA
HARMxEDADagrup = aov(MFQ_HARM_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
summary(HARMxEDADagrup)
# NO significativo

### Gráfico
# Ploteo
#library(ggplot2)
ggplot(data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"), 
       aes(x = factor(EDAD_agrupada, levels = orden5), 
           y = MFQ_HARM_AVG, fill = EDAD_agrupada, color = EDAD_agrupada, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("DAÑO/CUIDADO") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### FAIRNESS
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
FAIRNESSxEDADagrup_levene = leveneTest(MFQ_FAIRNESS_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
print(FAIRNESSxEDADagrup_levene)

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(Media = mean(MFQ_FAIRNESS_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(DesvioEstandar = sd(MFQ_FAIRNESS_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

# Corro el ANOVA
FAIRNESSxEDADagrup = aov(MFQ_FAIRNESS_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
summary(FAIRNESSxEDADagrup)
# Significativo

# Post-Hoc
FAIRNESSxEDADagrup_PH = TukeyHSD(FAIRNESSxEDADagrup)
print(FAIRNESSxEDADagrup_PH)
# Millennials-Generación Z

### Gráfico
# Ploteo
#library(ggplot2)
ggplot(data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"), 
       aes(x = factor(EDAD_agrupada, levels = orden5), 
           y = MFQ_FAIRNESS_AVG, fill = EDAD_agrupada, color = EDAD_agrupada, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("EQUIDAD/RECIPROCIDAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### INGROUP
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
INGROUPxEDADagrup_levene = leveneTest(MFQ_INGROUP_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
print(INGROUPxEDADagrup_levene)

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(Media = mean(MFQ_INGROUP_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(DesvioEstandar = sd(MFQ_INGROUP_AVG, na.rm = TRUE))

# Muestro los resultados
print(resultados8)
print(resultados9)

# Corro el ANOVA
INGROUPxEDADagrup = aov(MFQ_INGROUP_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
summary(INGROUPxEDADagrup)
# Significativo

# Post-Hoc
INGROUPxEDADagrup_PH = TukeyHSD(INGROUPxEDADagrup)
print(INGROUPxEDADagrup_PH)
# Generación Z-Generación X

### Gráfico
# Ploteo
#library(ggplot2)
ggplot(data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"), 
       aes(x = factor(EDAD_agrupada, levels = orden5), 
             y = MFQ_INGROUP_AVG, fill = EDAD_agrupada, color = EDAD_agrupada, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("GRUPO/LEALTAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### AUTHORITY
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
AUTHORITYxEDADagrup_levene = leveneTest(MFQ_AUTHORITY_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
print(AUTHORITYxEDADagrup_levene)

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(Media = mean(MFQ_AUTHORITY_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(DesvioEstandar = sd(MFQ_AUTHORITY_AVG, na.rm = TRUE))

# Veo los resultados
print(resultados8)
print(resultados9)

# Corro el ANOVA
AUTHORITYxEDADagrup = aov(MFQ_AUTHORITY_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
summary(AUTHORITYxEDADagrup)
# Significativo

# Post-Hoc
AUTHORITYxEDADagrup_PH = TukeyHSD(AUTHORITYxEDADagrup)
print(AUTHORITYxEDADagrup_PH)
# Generación Z-Generación X 
# Millennials-Generación X

### Gráfico
# Ploteo
ggplot(data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"), 
       aes(x = factor(EDAD_agrupada, levels = orden5), 
             y = MFQ_AUTHORITY_AVG, fill = EDAD_agrupada, color = EDAD_agrupada, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("AUTORIDAD/RESPETO") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)

###### PURITY
# Corroboro homogeneidad de la varianza dentro de los grupos => CUMPLE (resultado NO significativo)
PURITYxEDADagrup_levene = leveneTest(MFQ_PURITY_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
print(PURITYxEDADagrup_levene)

# Calculo la media y el desvío estándar
resultados8 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(Media = mean(MFQ_PURITY_AVG, na.rm = TRUE))

resultados9 = MFQ30 %>%
  group_by(EDAD_agrupada) %>%
  summarize(DesvioEstandar = sd(MFQ_PURITY_AVG, na.rm = TRUE))

# Veo los resultados
print(resultados8)
print(resultados9)

# Corro el ANOVA
PURITYxEDADagrup = aov(MFQ_PURITY_AVG ~ EDAD_agrupada, data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"))
summary(PURITYxEDADagrup)
# Significativo

# Post-Hoc
PURITYxEDADagrup_PH = TukeyHSD(PURITYxEDADagrup)
print(PURITYxEDADagrup_PH)
# Generación Z-Generación X
# Millennials-Generación X

### Gráfico
# Ploteo
ggplot(data = filter(MFQ30, EDAD_agrupada != "Baby Boomers"), 
       aes(x = factor(EDAD_agrupada, levels = orden5),
             y = MFQ_PURITY_AVG, fill = EDAD_agrupada, color = EDAD_agrupada, alpha = 0,7)) +
  geom_boxplot(notch = FALSE, linewidth = 0.9) +
  labs(x = "", y = "Puntaje") +
  guides(fill = "none", color = "none", alpha = "none") +
  theme_bw() +
  #ggtitle("PUREZA/SANTIDAD") +
  theme(#plot.title = element_text(family = "serif", size = 23, face = "bold"),
    axis.title = element_text(family = "serif", size = 17),
    axis.text.y = element_text(family = "serif", size = 19),
    axis.text.x = element_text(family = "serif", size = 21, face = "bold")) +
  geom_jitter(position = position_jitter(0.1)) +
  ylim(0,5)
