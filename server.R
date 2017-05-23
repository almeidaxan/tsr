library(shiny)

source("global.R")

shinyServer(function(input, output) {
	# acessar com s()
	s <- eventReactive(input$botaoLeitura, {
		withProgress(message = 'Reading input data...', value = NULL, {
			stack(paste0("data/", input$arquivo, ".grd"))
		})
	})

	# acessar com v()
	v <- eventReactive(input$botaoLeitura, {
		withProgress(message = 'Processing input data...', value = NULL, {
			temp <- extract(s(), 1:ncell(s()))
			temp[is.na(temp)] <- 0
			temp
		})
	})

	output$plot_teste <- renderPlot({
		par(mar=c(4,2,1,1)+0.1)
		v()
		image(s()[[1]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
	})

	# Aba Visualizacao
	# ----------------
		# outputSlider_1
		output$outputSlider_1 <- renderUI({
			sliderInput("t", "t", value = 1, min = 1, max = nlayers(s()), step = 1)
		})

		# outputSlider_2
		output$outputSlider_2 <- renderUI({
			sliderInput("x", "x", value = 1, min = 1, max = ncol(s()), step = 1)
		})

		# outputSlider_3
		output$outputSlider_3 <- renderUI({
			sliderInput("y", "y", value = 1, min = 1, max = nrow(s()), step = 1)
		})

		# plot da regiao de estudo
		output$plot_1 <- renderPlot({
			par(mar=c(4,2,1,1)+0.1)
			image(s()[[input$t]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s())[1],extent(s())[2]), labels=c(1,ncol(s())))
			axis(2, at=c(extent(s())[3],extent(s())[4]), labels=c(nrow(s()),1))
			points(extent(s())[1]+res(s())[1]*input$x, extent(s())[4]-res(s())[2]*input$y, pch=19, col="white", cex=1.5)
			points(extent(s())[1]+res(s())[1]*input$x, extent(s())[4]-res(s())[2]*input$y, pch=21, col="black", cex=1.5)
		})

		# plot da serie temporal dado um ponto (x,y)
		output$plot_2 <- renderPlot({
			par(mar=c(4,2,1,1)+0.1)
			plot(v()[input$x + ncol(s())*(input$y-1),], type="l", xlab="Time", ylab="NDVI", ylim=c(min(v()),max(v())))
			abline(v=input$t, col="red")
		})

	# Aba Consulta
	# ------------
		output$outputSlider_4 <- renderUI({
			sliderInput("x_querySlider", "x", value = 1, min = 1, max = ncol(s()), step = 1)
		})

		output$outputSlider_5 <- renderUI({
			sliderInput("y_querySlider", "y", value = 1, min = 1, max = nrow(s()), step = 1)
		})

		output$plot_3 <- renderPlot({
			par(mar=c(4,2,1,1)+0.1)
			image(s()[[input$t]], col=rev(terrain.colors(128)), xaxt="n", yaxt="n", xlab="", ylab="")
			axis(1, at=c(extent(s())[1],extent(s())[2]), labels=c(1,ncol(s())))
			axis(2, at=c(extent(s())[3],extent(s())[4]), labels=c(nrow(s()),1))
		})

		output$outputPlotClick <- renderPrint({
			list(x = round((as.double(input$plotClick_3$x)-extent(s())[1])/res(s())[1]),
				  y = round((extent(s())[4]-as.double(input$plotClick_3$y))/res(s())[2]))
		})

		output$plot_4a <- renderPlot({
			input$botaoConsulta

			withProgress(message = 'Processing query...', value = NULL, {
				isolate({
					if (input$tipo_entrada == "Specifying (x,y)") {
						qx <- input$x_querySlider
						qy <- input$y_querySlider
					} else {
						qx <- round((as.double(input$plotClick_3$x)-extent(s())[1])/res(s())[1])
						qy <- round((extent(s())[4]-as.double(input$plotClick_3$y))/res(s())[2])
					}

					q <- qx + ncol(s())*(qy-1)

					m <- matrix(0, nrow(s()), ncol(s()))
					dist <- c()

					if(length(qx) > 0 && length(qy) > 0) {
						if (input$algoritmo == "DTW Distance") {
							for (i in 1:ncell(s())) dist[i] <- distDTWC(v()[q,], v()[i,])
						} else if (input$algoritmo == "Manhattan Distance") {
							for (i in 1:ncell(s())) dist[i] <- distMinkC(v()[q,], v()[i,], 1)
						} else if (input$algoritmo == "Euclidian Distance") {
							for (i in 1:ncell(s())) dist[i] <- distMinkC(v()[q,], v()[i,], 2)
						} else if (input$algoritmo == "Chebyshev Distance") {
							for (i in 1:ncell(s())) dist[i] <- distChebC(v()[q,], v()[i,])
						} else if (input$algoritmo == "Cosine Similarity") {
							for (i in 1:ncell(s())) dist[i] <- simCosC(v()[q,], v()[i,])
						}

						# constroi a imagem de saida
						for(i in 1:nrow(s())) {
							for(j in 1:ncol(s())) {
								m[i,j] <- dist[j+(i-1)*ncol(s())]
							}
						}

						# gira a imagem de saida para plotar corretamente
						m <- t(m[nrow(m):1,])

						# plota imagem para diferentes limiares
						par(mar=c(4,2,1,1)+0.1)
						if(length(grep("similarity", input$algoritmo, ignore.case = T))>0) {
							image((m-min(m))/max(m-min(m)), col=colSim(29), xaxt="n", yaxt="n")
						} else {
							image((m-min(m))/max(m-min(m)), col=colDist(29), xaxt="n", yaxt="n")
						}
						axis(1, at=c(0,1), labels=c(1,ncol(s())))
						axis(2, at=c(0,1), labels=c(nrow(s()),1))
						if(length(grep("similarity", input$algoritmo, ignore.case = T))>0) {
							image.plot((m-min(m))/max(m-min(m)), col=colSim(29), xaxt="n", yaxt="n",
										  legend.only=T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="lesser           (Similarity)           greater")
						} else {
							image.plot((m-min(m))/max(m-min(m)), col=colDist(29), xaxt="n", yaxt="n",
										  legend.only=T, horizontal=T, smallplot=c(.23,.79,.12,.14),
										  legend.lab="closer              (Distance)              further")
						}

					}
				})
			})
		})
})