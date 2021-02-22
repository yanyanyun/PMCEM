##################################################### Integration ###########################################################
#load package
library(shiny)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(shinythemes)


function(input, output, session) {
  
  rsp_F <- reactive({
    req(input$dens)
    densi<-read.csv(input$dens$datapath, header=TRUE,stringsAsFactors=FALSE, fileEncoding="latin1")
    rs <- raster::raster(raster::extent(103.6051,104.0364,1.219747,1.472969), nrows=56,ncols=96)
    rs[] <- 1:raster::ncell(rs)
    rsp <- raster::rasterToPolygons(rs)
    sp::merge(rsp,densi,by.x="layer",by.y="layer", all.x=FALSE)
  })
  
  costi <- reactive({
    req(input$cost)
    read.csv(input$cost$datapath, header=TRUE)
  })
  
  binpal <- colorBin("Reds", c(0:14), 9, pretty = FALSE)
  
  output$map <- renderLeaflet({  
    
    rsp_x<-rsp_F()
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = rsp_x, 
                  fillColor = binpal(rsp_x$Predicted), 
                  fillOpacity = 0.3, 
                  color = "black", 
                  stroke = T, 
                  weight = 1, 
                  layerId = rsp_x$layer, 
                  group = "clickedIds", 
                  #label = rsp_x$Predicted
      )
    
    
  }) #END RENDER LEAFLET
  
  clickedIds <- reactiveValues(ids = vector())
  
  
  
  makeReactiveBinding("clickedPoly")
  
  observeEvent(input$map_shape_click, {
    
    rsp_x<-rsp_F()
    
    #create object for clicked polygon
    click <- input$map_shape_click
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #append all click ids in empty vector 
    clickedIds$ids <- c(clickedIds$ids, click$id)
    
    #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
    clickedPolys <- rsp_x[rsp_x$layer %in% clickedIds$ids, ]
    
    #if the current click ID [from CC_1] exists in the clicked polygon (if it has been clicked twice)
    if(click$id %in% clickedPolys$X.1){
      
      #define vector that subsets NAME that matches CC_1 click ID
      nameMatch <- clickedPolys$layer[clickedPolys$X.1 == click$id]
      
      #remove the current click$id AND its name match from the clickedPolys shapefile
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
      
      #remove that highlighted polygon from the map
      proxy %>% removeShape(layerId = click$id)
      
    } else {
      
      #map highlighted polygons
      proxy %>% addPolygons(data = clickedPolys,
                            fillColor = "black",
                            fillOpacity = 0.5,
                            weight = 1,
                            color = "black",
                            stroke = T,
                            #label = clickedPolys$Predicted, 
                            layerId = clickedPolys$X.1)
    } #END CONDITIONAL
    
    clickedPoly <<- subset(clickedPolys, clickedPolys$X.1!=click$id)
    
    average<-mean(clickedPoly$Predicted*3.73)
    
    output$aver<-renderText({
      
      average
      
    })
    
  }) #END OBSERVE EVENT
  
  
  observeEvent(input$calculate, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    
    rsp_x<-rsp_F()
    
    ##################################################### Function ###########################################################
    ## cost effectiveness management methods for covriates
    Capacity<-function(rate, ly, expect, time){
      
      r<-rate
      p<-rsp_x@data
      ply<-subset(p,p$layer==ly)
      N0=ply$Predicted
      Nt<-expect
      T<-time
      K<-(N0*(exp(r*T)-1))*Nt/(N0*exp(r*T)-Nt)  
      Delta<-log(N0)-log(K)
      Delta
      DeltaK<-Delta*1000000
      DeltaK<-as.integer(DeltaK)
      co<-costi()
      f=ply$FI
      e=ply$EE
      o=ply$OP
      o=as.integer(o*1000)
      b=ply$BS
      ra=ply$RA 
      baw=ply$BA*1000000
      ff=co[1,2]*T+co[1,3]
      ee=co[2,2]*T+co[2,3]
      oo=co[3,2]*T+co[3,3]
      bs=co[4,2]*T+co[4,3]
      aa=(co[5,2]*T*1+co[5,3]*1+co[5,4]*T*+co[5,5])*ra
      fff=co[1,4]*T+co[1,5]
      eee=co[2,4]*T+co[2,5]
      ooo=co[3,4]*T+co[3,5]
      bss=co[4,4]*T+co[4,5]
      aaa=0
      
      DFF<-data.frame("item"=c("feeding","eatingestablishment","overpass","busstop","buildingage"), "weight"=c(65000,3000,317,78000,baw), "unitvalue"=c(ff,ee,oo,bs,aa), "fixedvalue"=c(fff,eee,ooo,bss,aaa), "pieces"=c(f,e,o,b,1))
      
      fitness= function(x= rep(1, nrow(DFF))){
        total_value= sum(DFF$unitvalue * x + DFF$fixedvalue*round(x/(x+0.5)))
        total_weight= sum(DFF$weight * x)
        ifelse(total_weight >= DeltaK, total_value, Inf)
      }
      
      allowed= matrix(c(rep(0, nrow(DFF)), DFF$pieces), ncol = 2)
      set.seed(42)
      evolution= rgenoud::genoud(fn= fitness, 
                                 #'library(rgenoud)'in function change to 'rgenoud::'
                                 nvars= nrow(allowed), 
                                 max= FALSE,
                                 pop.size= 10000,
                                 data.type.int= TRUE, 
                                 Domains= allowed)
      
      cat("Value: ", evolution$value, "\n")
      cat("Weight:", sum(DFF$weight * evolution$par), "dag", "\n")
      best<-as.integer(evolution$par)
      besti<-c(ly, evolution$value, best)
      return(besti)
    }#######End function Capacity
    
    ##cost for culling
    #culling cost:unit time operational cost(2200)*number of culling operations(culling R Code)*T(24)
    
    #below is to count 'number of culling operations'
    cull <-function (rd, ly, t, Nt){
      p<-rsp_x@data
      ply<-subset(p,p$layer==ly)
      N0=ply$Predicted
      co<-costi()
      #population dynamics over time (t) when remove 10% of individuals
      N1 <- c(N0, numeric(t))
      for (i in 1:t) N1[i + 1] <- {
        N1[i] + rd * N1[i] * ((N0 - N1[i])/N0)-0.1*N1[i]
      }
      N1E<-tail(N1, n=1)
      
      #population dynamics over time (t) when remove 20% of individuals
      N2 <- c(N0, numeric(t))
      for (i in 1:t) N2[i + 1] <- {
        N2[i] + rd * N2[i] * ((N0 - N2[i])/N0)-0.2*N2[i]
      }
      N2E<-tail(N2, n=1)
      
      #population dynamics over time (t) when remove 30% of individuals
      N3 <- c(N0, numeric(t))
      for (i in 1:t) N3[i + 1] <- {
        N3[i] + rd * N3[i] * ((N0 - N3[i])/N0)-0.3*N3[i]
      }
      N3E<-tail(N3, n=1)
      
      #population dynamics over time (t) when remove 40% of individuals
      N4 <- c(N0, numeric(t))
      for (i in 1:t) N4[i + 1] <- {
        N4[i] + rd * N4[i] * ((N0 - N4[i])/N0)-0.4*N4[i]
      }
      N4E<-tail(N4, n=1)
      
      #population dynamics over time (t) when remove 50% of individuals
      N5 <- c(N0, numeric(t))
      for (i in 1:t) N5[i + 1] <- {
        N5[i] + rd * N5[i] * ((N0 - N5[i])/N0)-0.5*N5[i]
      }
      N5E<-tail(N5, n=1)
      
      #population dynamics over time (t) when remove 60% of individuals
      N6 <- c(N0, numeric(t))
      for (i in 1:t) N6[i + 1] <- {
        N6[i] + rd * N6[i] * ((N0 - N6[i])/N0)-0.6*N6[i]
      }
      N6E<-tail(N6, n=1)
      
      #population dynamics over time (t) when remove 70% of individuals
      N7 <- c(N0, numeric(t))
      for (i in 1:t) N7[i + 1] <- {
        N7[i] + rd * N7[i] * ((N0 - N7[i])/N0)-0.7*N7[i]
      }
      N7E<-tail(N7, n=1)
      
      #population dynamics over time (t) when remove 80% of individuals
      N8 <- c(N0, numeric(t))
      for (i in 1:t) N8[i + 1] <- {
        N8[i] + rd * N8[i] * ((N0 - N8[i])/N0)-0.8*N8[i]
      }
      N8E<-tail(N8, n=1)
      
      #population dynamics over time (t) when remove 90% of individuals
      N9 <- c(N0, numeric(t))
      for (i in 1:t) N9[i + 1] <- {
        N9[i] + rd * N9[i] * ((N0 - N9[i])/N0)-0.9*N9[i]
      }
      N9E<-tail(N9, n=1)
      
      #compiling all population sizes under different removal schemes after time (t)
      all<-c(N1E, N2E, N3E, N4E,N5E,N6E,N7E,N8E,N9E)
      all_c<-c(0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7, 0.8, 0.9)
      all_df <- data.frame(all,all_c)
      
      #keep removal schemes only have the size smaller than Nt after t
      all_df_min<-subset(all_df, all_df[,1]<=Nt)
      #choose the removal scheme with least removal effort
      all_final<-all_df_min[1,]
      #(co[6,2])is the cost for culling in 'Phase4>Rgenoud folder>cost.csv'
      #'all_final[1,2]'is the culling percentage this code result,0.3 is the 30% culling that vendor can do in one time operation.
      #'all_final[1,2]/0.3'is how many operations needed to cull required pigeon number.
      #cost equation: S=axT + bx + cT + d 
      CC<-all_final[1,2]/0.3*co[6,2]*t+all_final[1,2]/0.3*co[6,3]+co[6,4]*t+co[6,5]
      CCC<-c(ly, CC)
      return(CCC)
    }
    
    #k need to =N0, rd:growth rate; t:months; Nt:number after manage.
    
    #######End function culling
    
    ##################################################### ui_Output ###########################################################
    out_put<-data.frame(layer=numeric(),value=numeric(),feeding=integer(),eatingestablishment=integer(),overpass=integer(),busstop=integer(),buildingage=integer())
    
    out_put_c<-data.frame(layer=numeric(),value=numeric())
    
    HandledPoly<-subset(clickedPoly, clickedPoly$Predicted>=as.numeric(input$expct)/3.73)
    
    HandledPoly<-subset(HandledPoly, HandledPoly$V!="C")
    
    mult_cell <- HandledPoly$layer
    
    n.mult_cell <- length(mult_cell)
    
    #loop_capacity
    for (i in 1:n.mult_cell){
      
      out_put[i,] <- Capacity(rate = as.numeric(input$gr), ly = mult_cell[i], expect = as.numeric(input$expct)/3.73, time = as.numeric(input$mth))
      
    }
    
    sum_val<-sum(out_put$value)
    
    output$sum_v<-renderText({
      
      sum_val
      
    })#End loop_capacity
    
    #loop_culling
    for (i in 1:n.mult_cell){
      
      out_put_c[i,] <- cull(rd = as.numeric(input$gr), ly = mult_cell[i], t = as.numeric(input$mth), Nt = as.numeric(input$expct)/3.73)
      
    }
    
    sum_val_c<-sum(out_put_c$value)
    
    output$sum_c<-renderText({
      
      sum_val_c
      
    })#End loop_culling
    
    removeModal()
    
    #convert dataframe to spatialpolygondataframe
    rs <- raster::raster(raster::extent(103.6051,104.0364,1.219747,1.472969), nrows=56,ncols=96)
    rs[] <- 1:raster::ncell(rs)
    rsp <- raster::rasterToPolygons(rs)
    polycap<-sp::merge(rsp,out_put,by.x="layer",by.y="layer", all.x=FALSE)
    
    ####downloadButton
    output$downloadData<- downloadHandler(
      filename = function() {
        paste('data','.kml',sep='')
      },
      content = function(file) {
        rgdal::writeOGR(polycap,file,layer="Management",driver="KML")
      }
    )#end downloadbutton
    
    
  })#END OBSERVE EVENT
  
}
