if(!require("agricolae")) install.packages("agricolae")
if(!require("desplot")) install.packages("desplot")
trt <- c("Control", "N50", "N100", "N150")
outdesign <- design.rcbd(trt, r = 4, serie = 2) # 4 repeticiones
field_book <- outdesign$book
field_book

# Asigna columnas y filas: ajusta para que cada bloque (rep) ocupe filas consecutivas
# Ejemplo: rep 1 en filas 1-4, rep 2 en 5-8, etc. (total 16 filas para 4 reps)
field_book$col <- rep(1:4, times = 4)  # Columnas 1-4 repetidas
field_book$row <- rep(1:4, each = 4)  # Filas 1-16, una por parcela (para separar bloques claramente)
field_book

# Genera el plot base con desplot

windows(10,10)
p <- desplot(field_book, trt ~ col * row, 
             text = trt, cex = 1, 
             main = "Croquis de Campo: Ensayo de Fertilización Nitrogenada",
             show.key = TRUE)
print(p)
trellis.focus("panel", 1, 1)
panel.abline(h = seq(1.5, 3.5, by = 1), lwd = 2, col = "black")
trellis.unfocus()
