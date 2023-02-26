
#remotes::install_github("Pierrepierrepierrepierrepierrepierre/heron")
library(heron)

# Fonction : centre_segment

#' centre_segment 
#' @description Cette fonction calcule le milieu d'un segment 
#' @param x_a Coordonnées x d'un premier point 
#' @param y_a Coordonnées y d'un premier point 
#' @param x_b Coordonnées x d'un second point 
#' @param y_b Coordonnées y d'un second point 
#' @return Retourne un vecteur (x,y) des coordonnnées du centre d'un segment
#' @noRd
#' @examples
centre_segment <- function(x_a,y_a,x_b,y_b){ 
  return(c((x_a+x_b)/2,(y_a+y_b)/2))
}

# Fonction : divide_triangle

#' divide_triangle 
#' @description Cette fonction permet de diviser un triangle en 4 triangles  à l'aide du centre de chacun des segments. 
#' 
#' @param vect 
#'
#' @return 
#' @export
divide_triangle <-
  function(vect_coord = c(x_a, y_a, x_b, y_b, x_c, y_c)) {
    c_ab <-
      centre_segment(vect_coord[1], vect_coord[2], vect_coord[3], vect_coord[4])
    c_ac <-
      centre_segment(vect_coord[1], vect_coord[2], vect_coord[5], vect_coord[6])
    c_bc <-
      centre_segment(vect_coord[3], vect_coord[4], vect_coord[5], vect_coord[6])
    # Créer les nouveaux triangles
    t_1 <-
      c(vect_coord[1], vect_coord[2], c_ab[1], c_ab[2], c_ac[1], c_ac[2])
    t_2 <-
      c(vect_coord[3], vect_coord[4], c_ab[1], c_ab[2], c_bc[1], c_bc[2])
    t_3 <-
      c(vect_coord[5], vect_coord[6], c_ac[1], c_ac[2], c_bc[1], c_bc[2])
    return(list(t_1, t_2, t_3))
  }


# Fonction : divide_liste_triangle

#' divide_liste_triangle 
#' @description Cette fonciton prend une liste de triangle et renvoie une liste 
#' composé des 4 triangles
#' @param liste_triangle Une liste de coordonées de triangles 
#'
#' @return On applique l'algorithme de Sierpinski aux 
#' triangles donnés en entrée. 
#' @export
#'
divide_list_triangle <- function(liste_triangle){ 
  sous_liste <- list()
  for (i in liste_triangle){ 
    sous_liste <- append(sous_liste,divide_triangle(i))
  }
  return(sous_liste)
}


#Question 5
install.packages("remotes")

#C'est ce qu'il auraitt fallu que je fasse si j'avais réussi à créer le package héron.
#remotes::install_github("git@github.com:Pierrepierrepierrepierrepierrepierre/heron.git")

#  > remotes::install_github("git@github.com:Pierrepierrepierrepierrepierrepierre/heron.git")
#Error: Failed to install 'unknown package' from GitHub:
#  Line starting ':) ...' is malformed!
#  > remotes::install_github("https://github.com/Pierrepierrepierrepierrepierrepierre/heron")
#Error: Failed to install 'unknown package' from GitHub:
#  Line starting ':) ...' is malformed!


# Fonction : plot_triangles

#' plot_triangles 
#' @description #' Cette fonction permet de tracer tous les triangles à partir de leurs coordonnées
#' @import ggplot2 
#' @param liste_triangle le premier paramètres est une liste de triangles comme ci dessous : 
#' liste_triangle = list([x11,y11,x21,y21,x31,y31],[x12,y12,x22,y22,x32,y32] ... ). 
#'
#' @return Cette fonction affiche les 4 trianles. 
#' @export 
plot_triangles <- function(liste_triangle) {
  df <- data.frame()
  
  # Data frame à partir des coordonnées des triangles
  for (i in seq_along(liste_triangle)) {
    df <- rbind(df, data.frame(
      x = c(liste_triangle[[i]][1], liste_triangle[[i]][3], liste_triangle[[i]][5]),
      y = c(liste_triangle[[i]][2], liste_triangle[[i]][4], liste_triangle[[i]][6]),
      triangle_id = i
    ))
  }
  
  

# Définition de la fonction heron_liste qui prend en entrée une liste de triangles
heron_liste <- function(liste_triangle) {
  # Initialisation de la variable qui stockera la somme des aires
  sum_aire <- 0
  
  # Boucle sur chaque triangle dans la liste
  for (i in seq_along(liste_triangle)) {
    # Extraction des distances entre les sommets du triangle i
    A_B <- distance_cart(liste_triangle[[i]][1], liste_triangle[[i]][2], liste_triangle[[i]][3], liste_triangle[[i]][4])
    B_C <- distance_cart(liste_triangle[[i]][3], liste_triangle[[i]][4], liste_triangle[[i]][5], liste_triangle[[i]][6])
    C_A <- distance_cart(liste_triangle[[i]][5], liste_triangle[[i]][6], liste_triangle[[i]][1], liste_triangle[[i]][2])
    
    # Calcul de l'aire du triangle i en utilisant la formule de Héron
    aire_i <- heron(A_B, B_C, C_A)
    
    # Ajout de l'aire du triangle i à la somme des aires
    sum_aire <- sum_aire + aire_i
  }
  
  # Retourne la somme des aires
  return(sum_aire)
}


#On trace les triangles
plot <- ggplot(df, aes(x, y, fill = factor(triangle_id))) +
  geom_polygon(color = "black") +
  scale_fill_manual(values = rep("black", length(liste_triangle)), guide = "none") +
  theme_void()

return(plot)
}


