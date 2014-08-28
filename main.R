###
# main script
###

# libs:
library(plyr)
library(ggplot2)
library(plotrix)
library(stringr)
library(grid)

# utils/funs/etc:
source('mapping.R')
source('functions.R')

# global conf:
da_answer = 'ΔΑ'
options(stringsAsFactors = FALSE)
csvsfolder <- "csvfiles/"
csvfiles <- c('orash/patra.csv',
              'orash/bolos.csv',
              'orash/lista.csv',
              'orash/nauplio.csv',
              'akoh/patra.csv',
              'trith-hlikia/kaph-agia.csv',
              'trith-hlikia/kaph-jaxou.csv',
              'trith-hlikia/kaph-prosfygika.csv',
              'trith-hlikia/oikos-eughrias-aigio.csv',
              'trith-hlikia/oikos-eughrias-patra.csv'
              )
timestamp_format <- "%Y%m%d_%H%M%S"
outputfolder <- "output/"
unlink_and_create_folder(outputfolder)

# orasi all:
questions_to_hist_plot_orash <- c("Ερώτηση.2","Ερώτηση.6")
questions_to_sort_orash <- c("Ερώτηση.3","Ερώτηση.9","Ερώτηση.14","Ερώτηση.15","Ερώτηση.16","Ερώτηση.20","Ερώτηση.22","Ερώτηση.23","Ερώτηση.24","Ερώτηση.25","Ερώτηση.28","Ερώτηση.32")
questions_to_pie_orash <- c("Προέλευση","Ερώτηση.1")
questions_to_grouplot_orash <- c("Ερώτηση.10","Ερώτηση.19")
questions_to_barplot_orash <- names(full_questions_orash)[! names(full_questions_orash) %in% c(questions_to_hist_plot_orash,questions_to_pie_orash,questions_to_grouplot_orash)]
do_all(csvsfolder,csvfiles,1:4,outputfolder,"orash-all/",full_questions_orash,full_answers_orash,questions_to_barplot_orash,questions_to_hist_plot_orash,questions_to_sort_orash,questions_to_pie_orash,questions_to_grouplot_orash)

# akoh:
questions_to_hist_plot_akoh <- c("Ερώτηση.2","Ερώτηση.6")
questions_to_pie_akoh <- c("Προέλευση","Ερώτηση.1")
questions_to_grouplot_akoh <- c("Ερώτηση.13","Ερώτηση.15","Ερώτηση.24")
questions_to_sort_akoh <- c("Ερώτηση.3","Ερώτηση.19","Ερώτηση.21","Ερώτηση.25","Ερώτηση.27","Ερώτηση.28","Ερώτηση.29","Ερώτηση.30","Ερώτηση.33","Ερώτηση.38")
questions_to_barplot_akoh <- names(full_questions_akoh)[! names(full_questions_akoh) %in% c(questions_to_hist_plot_akoh,questions_to_pie_akoh,questions_to_grouplot_akoh)]
do_all(csvsfolder,csvfiles,c(5),outputfolder,"akoh-all/",full_questions_akoh,full_answers_akoh,questions_to_barplot_akoh,questions_to_hist_plot_akoh,questions_to_sort_akoh,questions_to_pie_akoh,questions_to_grouplot_akoh)

# 3h hlikia:
questions_to_hist_plot_3hlikia <- c("Ερώτηση.2")
questions_to_pie_3hlikia <- c("Προέλευση","Ερώτηση.1")
questions_to_grouplot_3hlikia <- c("Ερώτηση.6","Ερώτηση.14")
questions_to_sort_3hlikia <- c("Ερώτηση.3","Ερώτηση.15","Ερώτηση.17","Ερώτηση.18","Ερώτηση.19","Ερώτηση.20","Ερώτηση.23","Ερώτηση.27")
questions_to_barplot_3hlikia <- names(full_questions_3hlikia)[! names(full_questions_3hlikia) %in% c(questions_to_hist_plot_3hlikia,questions_to_pie_3hlikia,questions_to_grouplot_3hlikia)]
do_all(csvsfolder,csvfiles,6:10,outputfolder,"3h-hlikia-all/",full_questions_3hlikia,full_answers_3hlikia,questions_to_barplot_3hlikia,questions_to_hist_plot_3hlikia,questions_to_sort_3hlikia,questions_to_pie_3hlikia,questions_to_grouplot_3hlikia)

print('END')
