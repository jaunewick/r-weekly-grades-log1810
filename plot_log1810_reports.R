library(magrittr)

path <- getwd()
noteCP2 <- read.csv(paste(path, "/grades_log1810.csv", sep = ""))

groups <- list(
  group1 = noteCP2[, 1],
  group2 = noteCP2[, 2],
  group3 = noteCP2[, 3][-c(51:55)]
)
plot_group <- function(data, group_name, color) {
  h <- hist(
    data,
    breaks = 10,
    main = paste("Notes des TD -", group_name),
    xlab = "Note sur 10",
    ylab = "Fréquence",
    col = color,
    ylim = c(0, 30),
  )
  h
  text(h$mids, h$counts + 1, ifelse(h$counts == 0, "", h$counts))
  abline(v = mean(data), col = "red", lwd = 2, lty = 2) # Mean
  abline(v = median(data), col = "blue", lwd = 2, lty = 2) # Median

  legend("topleft",
    legend = c(
      sprintf("Médiane = %.2f", median(data)),
      sprintf("Moyenne = %.2f", mean(data)),
      sprintf("Écart type = %.2f", sd(data))
    ),
    col = c("blue", "red", "white"),
    lwd = 2
  )

  boxplot(
    data,
    horizontal = TRUE,
    notch = TRUE,
    frame = FALSE,
    col = color,
    xlab = ""
  )
}

layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), height = c(4, 2))

plot_group(groups$group1, "Groupe 04L", "thistle1")
plot_group(groups$group2, "Groupe 01L", "palegreen")
plot_group(groups$group3, "Groupe 03L", "lightsteelblue1")
