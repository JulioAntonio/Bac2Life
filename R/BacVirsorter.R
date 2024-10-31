#' VirSorter Table Preparation for CIRCOS
#'
#' @author Dr. Abraham Cruz, Dr. Julio A. Hernandez-Gonzalez
#'
#' @description
#' The BacVirsorter function processes VirSorter and karyotype data files to create a formatted table for CIRCOS visualization.
#'
#'
#' @param virsorter_file: The file path to the VirSorter output file containing scaffold, position and scoring information.
#' @param karyotype_file: The file path to the karyotype file containing scaffold and chromosomal group data.
#'
#' @return A data.frame with scaffold information mapped to CORCOS formatting, including RGB color assigments and position data.
#' @export
#'
#' @examples
BacVirsorter <- function(virsorter_file, karyotype_file) {
  require(readr)
  require(dplyr)

  # Leer y procesar el archivo virsorter
  virsorter <- read_table(virsorter_file, header = TRUE) %>%
    select(seqname, trim_bp_start, trim_bp_end, pr_full, hallmark_cnt, final_max_score_group)
  colnames(virsorter) <- c("scaffold", "start", "end", "score", "hallmark_cnt", "Phage group")

  # Leer y procesar el archivo karyotype
  karyotype <- read_table(karyotype_file, col_names = FALSE)
  colnames(karyotype) <- c("chr", "dash", "name", "scaffold", "start", "end", "chr_group")

  # Crear un mapeo de scaffold a su equivalente
  mapping <- karyotype %>%
    select(scaffold, name) %>%
    distinct()

  # Adicionar name (axis) al unir con virsorter
  combined <- virsorter %>%
    left_join(mapping, by = "scaffold")

  # Reorganizar el DataFrame
  combined <- combined %>%
    select(equivalent = name, start, end, `Phage group`, score)

  # Definir mapa de colores RGB
  color_map_rgb <- c("high_score" = "144,238,144",  # Verde claro
                     "low_score" = "255,255,224")   # Amarillo claro

  # Filtrar y transformar el `data.frame` para generar el formato Circos con colores RGB
  formatted_circos <- combined %>%
    mutate(
      # Asignar el color en formato RGB según el score
      fill_color = ifelse(score >= 0.9, color_map_rgb["high_score"], color_map_rgb["low_score"]),
      # Definir r_values según la condición
      r_values = "r0=0.65r,r1=0.65r+50p",
      # Crear la columna final con el formato Circos, incluyendo los valores RGB
      circos_format = paste(equivalent, start, end, `Phage group`,
                            paste0("fill_color=", fill_color, ",", r_values), sep = " ")
    )

  # Guardar el archivo en el formato Circos requerido
  write.table(formatted_circos$circos_format, "Highlight5.tmp",
              row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Retornar el DataFrame transformado
  return(formatted_circos)
}
