# Group 2                       BSCOE 1-1             Engineering Data Analysis
# Lim, Shin I.
# Ilao,Kenji C.
# Esguerra,Edgar Jr. P.
# Mina, Siegfred Lorelle C.
# Agcaoili, Leon Adriel Franco M.

# Group Bonus Assessment #5


library(ggplot2)
attach(mpg)

mpg <- mpg

colors_of_dot = c("red", "red2","red3", 
                    "gold1", "gold3", "goldenrod", "goldenrod2", "goldenrod3",
                    "slateblue", "slateblue1", "slateblue2", "slateblue3", "slateblue4",
                    "hotpink", "hotpink1", "hotpink2", "hotpink3", "hotpink4",
                    "gray", "gray20", "gray40", "gray60", "gray80",
                    "navy", "navyblue",
                    "salmon1","salmon2")

# MAIN
print(
    ggplot(mpg, aes(hwy, displ)) + 
        geom_point(aes(colour = factor(hwy)),
                        size = 3.5,
                        shape = 8) +
        scale_colour_manual("\nHighway mi. per gal\n",
                            values = colors_of_dot,
                            aesthetics = c("colour")) +
        labs(x = "\nHighway Miles per Gallon\n", 
            y = "\nEngine Displacement (Liters)\n") +

        # ADDITIONAL
        theme_linedraw() +
        ggtitle("\nScatterplot of Highway Miles per Gallon vs Engine Displacement (in Liters)\n")
    )


detach(mpg)

print("View the scatter plot in the plot panel in Rstudio.")