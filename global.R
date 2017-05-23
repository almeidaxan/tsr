library(shiny)
library(raster) # mexer com imagens em raster/stacks
library(Rcpp) # para rodar certos codigos em C++
library(fields) # image.plot = plot de imagem com legenda de cor

# ////////////////////////////////////////////////////////////////

# # SUGESTOES PARA ALTERACAO:

# - Na aba de Visualização, permitir que o usuário especifique o
#   par de coordenadas atraves do input do mouse.

# - Na aba de Consulta, permitir que o usuário _visualize_ o ponto
#   (x,y) escolhido em uma imagem.

# - Possibilidade de escolher a color palette da consulta on-the-fly.

# - Quando oportuno, verificar metodos da literatura para desconsiderar
# nuvens no produto NDVI e/ou outros e incluir no sistema.

# ////////////////////////////////////////////////////////////////

# ------
# INICIO
# ------

# leitura do nome de todos os arquivos ".grd" na wd
arq <- dir(path = "data", pattern = ".grd")
arq <- strtrim(arq, nchar(arq)-4)

# cores customizadas para plotar os graficos da consultas
colDist <- colorRampPalette(c("red", "white", "green"))
colSim <- colorRampPalette(c("green", "white", "red"))

# -----------------
# FUNCOES AUXILARES
# -----------------

# distancia DTW (escrita em C)
cppFunction('double distDTWC(NumericVector x, NumericVector y) {
					int nrow = x.size(), ncol = y.size();
					double cost;
					double min;
					NumericMatrix d(nrow+1,ncol+1);

					for(int i = 0; i < nrow+1; ++i)
						d(i,0) = INFINITY;
					for(int j = 0; j < ncol+1; ++j)
						d(0,j) = INFINITY;
					d(0,0) = 0;

					for(int i = 1; i < nrow+1; ++i) {
						for(int j = 1; j < ncol+1; ++j) {
							cost = sqrt(pow(x[i-1],2.0) + pow(y[j-1],2.0));

							min = d(i-1,j);
							if(d(i,j-1)<min) min = d(i,j-1);
							if(d(i-1,j-1)<min) min = d(i-1,j-1);

							d(i,j) = cost + min;
						}
					}

					return d(nrow,ncol);
				}')

# distancia generalizada de Minkowski (escrita em C)
cppFunction('double distMinkC(NumericVector x, NumericVector y, double pot) {
					int n = y.size();
					double out = 0;

					for(int i = 0; i < n; ++i) {
						out = out + pow(fabs(y[i] - x[i]), pot);
					}

					out = pow(out, 1/pot);

					return out;
				}')

# distancia de Chebyshev [Minkowski quando pot->Inf] (escrita em C)
cppFunction('double distChebC(NumericVector x, NumericVector y) {
					int n = y.size();
					double out = 0;

					for(int i = 0; i < n; ++i) {
						if(fabs(y[i] - x[i]) > out)
							out = fabs(y[i] - x[i]);
					}

					return out;
				}')

# similaridade do cosseno (escrita em C)
cppFunction('double simCosC(NumericVector x, NumericVector y) {
					int n = y.size();
					double dot = 0.0;
					double denom_a = 0.0;
					double denom_b = 0.0;

					for(int i = 0; i < n; ++i) {
						dot += x[i]*y[i];
						denom_a += x[i]*x[i];
						denom_b += y[i]*y[i];
					}

					return dot / (sqrt(denom_a) * sqrt(denom_b));
				}')

