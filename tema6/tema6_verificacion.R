# ==========================================
# 1. CONSTRUCCIÓN DE DATOS (Tu código original)
# ==========================================
# Reconstruye el degradado con matrix()
fila <- seq(0, 250, length.out = 5)
gris <- matrix(rep(fila, each = 5), nrow = 5, ncol = 5, byrow = TRUE)

cat("Dimensiones (dim):", dim(gris), "\n")
cat("Promedio (mean):", mean(gris), "\n")

# Reconstruye la imagen RGB de dos mitades
rgb <- array(0, dim = c(4, 4, 3))
rgb[, 1:2, 1] <- 0   # R
rgb[, 1:2, 2] <- 255 # G
rgb[, 1:2, 3] <- 0   # B

rgb[, 3:4, 1] <- 0   # R
rgb[, 3:4, 2] <- 0   # G
rgb[, 3:4, 3] <- 255 # B

cat("Píxel [1,1] (Verde):", rgb[1, 1, ], "\n")
cat("Píxel [1,4] (Azul):", rgb[1, 4, ], "\n")

# Gris por luminosidad ponderada
gris_luminosidad <- 0.299 * rgb[, , 1] + 0.587 * rgb[, , 2] + 0.114 * rgb[, , 3]
cat("\nGris Luminosidad en R:\n")
print(gris_luminosidad)


# ==========================================
# 2. GENERACIÓN DE GRÁFICOS
# ==========================================

# Configurar la ventana de gráficos para mostrar 2x2 (4 gráficos)
par(mfrow = c(2, 2), mar = c(2, 2, 3, 1)) 

# --- GRÁFICO 1: Degradado en escala de grises ---
# Usamos image() y giramos la matriz para que se vea como en Python
image(t(gris)[, nrow(gris):1], 
      col = gray.colors(256), 
      main = "1. Degradado 5x5", 
      axes = FALSE)
box()

# --- GRÁFICO 2: Imagen RGB (Verde y Azul) ---
# Convertimos el array a un objeto raster para poder graficarlo
img_rgb <- as.raster(rgb / 255) 
plot(img_rgb, 
     main = "2. Imagen RGB (Verde y Azul)", 
     axes = FALSE)

# --- GRÁFICO 3: Imagen en escala de grises (Luminosidad) ---
image(t(gris_luminosidad)[, nrow(gris_luminosidad):1], 
      col = gray.colors(256), 
      main = "3. Gris por Luminosidad", 
      axes = FALSE)
box()

# --- GRÁFICO 4: Binarización ---
# Definimos un umbral (por ejemplo 100) y binarizamos
umbral <- 100
binaria <- gris_luminosidad > umbral

image(t(binaria)[, nrow(binaria):1], 
      col = c("black", "white"), 
      main = paste("4. Binarizada (Umbral =", umbral, ")"), 
      axes = FALSE)
box()

# Restaurar la configuración original de los gráficos
par(mfrow = c(1, 1))

