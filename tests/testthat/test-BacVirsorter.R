test_that("BacVirsorter works", {
  #importar archivos de entrada
  virsorter <- readr::read_table("data/virsorter.tsv")
  karyotype <- readr::read_table("data/karyotype.tsv", col_names = F)
  #correr funcion
  formatted_circos <- BacVirsorter(virsorter, karyotype)
  #numero de columnas esperado
  expected <- 8
  #numero de columnas del archivo de salida
  actual <- ncol(formatted_circos)
  #verificar que los num columnas coincidan
  expect_equal(expected, actual)
})
