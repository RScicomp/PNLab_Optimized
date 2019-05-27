

#title: "Eye Tracking Study"
#output: html_notebook
#author: Richard Gao for PNLab
user <- dlgInput("1: Open Master
                 2: Collect Stats
                 3: HeatMap", default = "", gui=.GUI )
source("Simpleinterface.R")
dlg_input(message = "Enter a value", default = "", gui = .GUI)
shinyApp(ui = ui, server = server)

