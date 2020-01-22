








## 3D Map ##


devtools::install_github("tylermorganwall/rayshader")

library(rayshader)

#Here, I load a map with the raster package.
loadzip = tempfile()
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

montshadow = ray_shade(montereybay, zscale = 50, lambert = FALSE)
montamb = ambient_shade(montereybay, zscale = 50)
montereybay %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb, 0) %>%
  plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(0.2)
render_snapshot(clear=TRUE)

data(ak)



akshadow = ray_shade(ak, zscale = 50, lambert = FALSE)
akamb = ambient_shade(ak, zscale = 50)
montereybay %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb, 0) %>%

    plot_3d(ak, zscale = 50, fov = 0, theta = -45, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5)





