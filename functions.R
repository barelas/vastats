aggr_count <- function(df,col) {
  length(df[[col]])
}

csvs_as_df <- function(csvsfolder,csvfiles,indices) {
  res <- data.frame()
  for (i in indices) {
    csvfile <- csvfiles[i]
    df <- read.csv(paste0(csvsfolder,csvfile), strip.white = TRUE, quote = "")
    df$Προέλευση <- rep(i,nrow(df))
    res <- rbind(res,df)
  }
  return(res)
}

calc_stats <- function(csvsfolder,csvfiles,indices,df) {
  res <- list()
  all_count <- nrow(df)
  print(paste('Τυπική απόκλιση ηλικίας:',round(sd(df[['Ερώτηση.2']],na.rm = TRUE),1)))
  df <- fix_data(df)
  cols = colnames(df)
  for (col in cols) {
    count <- ddply(df, col, aggr_count, col)
    count <- count[!count[1]==-1,]
    count$pososto <- paste0(round(count[[2]]*100/all_count,1),'%')
    res[[col]]$count <- count
#     res[[col]]$mean <- mean(df[[col]])
    res[[col]]$count_nas <- sum(is.na(df[[col]]))
    res[[col]]$count_valids <- sum(!is.na(df[[col]]))
    res[[col]]$question_name <- col
    res[[col]]$all_count <- all_count
  }

  return(res)
}

unlink_and_create_folder <- function(folderpath) {
  if (file.exists(folderpath)) {
    unlink(folderpath,recursive = TRUE)
  }
  dir.create(folderpath, recursive = TRUE)
}

make_barplots <- function(cs,outputfolder,full_questions,full_answers,questions_to_barplot,questions_to_sort) {
  unlink_and_create_folder(outputfolder)
  for (question_to_barplot in questions_to_barplot) {
    question <- cs[[question_to_barplot]]
    plotpath <- get_plotpath(outputfolder,question$question_name)
    q_count <- question$count
    if (question_to_barplot %in% questions_to_sort) {
      q_count <- q_count[order(q_count[2]),]
    } else {
      q_count <- q_count[nrow(q_count):1,]
    }
    bar_names <- get_bar_names(q_count[[1]],full_answers[[question$question_name]])
    png(plotpath,width = 500, height = 208 + 14*length(bar_names), pointsize = 10)
    subtitle <- sprintf("Αριθμός ερωτηματολογίων: %d",question$all_count)
    left_margin <- round(max(nchar(bar_names))*0.4051 + 2.3418)
    par(mar = c(3.1, left_margin, 6, 0.7))
    bar_colors <- rev(terrain.colors(length(q_count[[1]])))
    bplot <- barplot(
      q_count[[2]],
      names = bar_names,
      horiz = TRUE,
      las = 1,
      cex.names = 0.95,
      xlim = c(0,max(q_count[[2]]) + 1),
      border = NA,
      col = bar_colors
    )
    kk <- 255 - col2rgb(bar_colors)
    text_colors <- rgb(red = kk[1,], green = kk[2,], blue = kk[3,], maxColorValue = 255)
    text(0, bplot, paste(q_count[[2]],q_count$pososto), pos = 4)
    title(
      main = strwrap(get_full_question(question$question_name,full_questions), round(83.421-2.105*left_margin)),
#       ylab = "Επιλογές", 
#       xlab = "Αριθμός απαντήσεων",
      sub = subtitle,
      line = 2
    )
    dev.off()
  }
}

make_grouplots <- function(cs,outputfolder,full_questions,full_answers,questions_to_grouplot) {
  for (question_to_grouplot in questions_to_grouplot) {
    question <- cs[[question_to_grouplot]]
    plotpath <- get_plotpath(outputfolder,question$question_name)
    options <- full_answers[[question_to_grouplot]]$options
    levels <- full_answers[[question_to_grouplot]]$levels
    q_count <- question$count
    da_count <- q_count[q_count[1]==da_answer,2][[1]]
    q_count <- q_count[q_count[1]!='',]
    q_count <- q_count[q_count[1]!=da_answer,]
    plot_matrix <- matrix(
      data = rep(0,length(levels)*length(options)),
      nrow = length(options),
      ncol = length(levels),
      dimnames = list(unlist(options),unlist(levels))
    )
    for (i in 1:nrow(q_count)) {
      row <- q_count[i,]
      digits <- as.numeric(unlist(strsplit(as.character(row[[1]]), "")))
      plot_matrix[digits[1],digits[2]] <- row[[2]]
    }
    png(plotpath,width = 500, height = 400, pointsize = 10)
    subtitle <- sprintf("Αριθμός ερωτηματολογίων: %d ΔΑ: %d",question$all_count,da_count)
    bplot <- barplot(
      plot_matrix,
      legend = options,
      cex.names = 0.95,
      beside = TRUE,
      col = topo.colors(length(options))
    )
    title(
      main = strwrap(get_full_question(question$question_name,full_questions), 100),
      xlab = "Επίπεδο",
      ylab = "Αριθμός απαντήσεων",
      sub = subtitle
    )
    dev.off()
  }
}

make_pies <- function(cs,outputfolder,full_questions,full_answers,questions_to_pie) {
  for (question_to_pie in questions_to_pie) {
    question <- cs[[question_to_pie]]
    plotpath <- get_plotpath(outputfolder,question$question_name)
    q_count <- question$count
    bar_names <- get_bar_names(q_count[[1]],full_answers[[question$question_name]])
    png(plotpath,width = 500, height = 400, pointsize = 10)
    subtitle <- sprintf("Αριθμός ερωτηματολογίων: %d",question$all_count)
    pie_plot <- pie3D(
      q_count[[2]],
      labels = paste0(bar_names," (",q_count[[2]]," - ",q_count$pososto,")"),
      main = strwrap(get_full_question(question$question_name,full_questions), 100),
      col = topo.colors(length(q_count[[1]])),
      explode = 0.02,
      cex.names = 0.7,
      theta = pi/2.5
    )
    mtext(subtitle,side = 1,line = 3)
    dev.off()
  }
}

get_plotpath <- function(outputfolder,question_name) {
  paste0(outputfolder,question_name,"_count_plot.png")
}

get_full_question <- function(question_name,full_questions) {
  full_question <- question_name
  if (question_name %in% names(full_questions)) {
    full_question <- sprintf("%s: %s",question_name,full_questions[question_name])
  }
  return(full_question)
}

get_bar_names <- function(raw_names,full_answers) {
#   str(full_answers)
  res = c()
  for (raw_value in raw_names) {
    if (raw_value %in% names(full_answers)) {
      res <- c(res,full_answers[[raw_value]])
    } else {
      res <- c(res,raw_value)
    }
  }
  return(res)
}

fix_data <- function(df) {
  for (rownum in 1:nrow(df)) {
    for (colnum in 1:ncol(df)) {
      val <- df[rownum,colnum]
      if (is.na(val) || ''==str_trim(val)) {
        df[rownum,colnum] <- da_answer
      } else {
        num_val <- as.numeric(val)
        if (is.na(num_val)) {
          # we have a multiple choice, add rows of NAs for correct counting:
          first_done <- FALSE
          for (mval in strsplit(val," ")[[1]]) {
            if (first_done) {
              newrow = rep(-1,ncol(df))
              newrow[colnum] <- as.numeric(mval)
              df <- rbind(df,newrow)
            } else {
              df[rownum,colnum] <- as.numeric(mval)
              first_done <- TRUE
            }
          }
        } else {
          df[rownum,colnum] <- num_val
        }
      }
    }
  }
  return(df)
}

do_all <- function(csvsfolder,csvfiles,indices,outputfolder,output_subfolder,full_questions,full_answers,questions_to_barplot,questions_to_hist_plot,questions_to_sort,questions_to_pie,questions_to_grouplot) {
  df <- csvs_as_df(csvsfolder,csvfiles,indices)
  cs <- calc_stats(csvsfolder,csvfiles,indices,df)
  out_folder <- paste(outputfolder,output_subfolder)
  make_barplots(cs,out_folder,full_questions,full_answers,questions_to_barplot,questions_to_sort)
  make_grouplots(cs,out_folder,full_questions,full_answers,questions_to_grouplot)
  make_pies(cs,out_folder,full_questions,full_answers,questions_to_pie)
  for (question_to_hist_plot in questions_to_hist_plot) {
    make_hist(cs,question_to_hist_plot,full_questions,out_folder)
  }
}

make_hist <- function(cs,question,full_questions,out_folder) {
  counts <- cs[[question]]$count
  da_count <- counts[counts[1]==da_answer,2]
  if (!any(da_count)) {
    da_count <- 0L
  }
  counts <- counts[counts[1]!=da_answer,]
  # 200 code means later than birth, but no age given.
  count_200 <- counts[counts[1]=='200',2]
  counts <- counts[counts[1]!='200',]
  xvals <- as.numeric(counts[[question]])
  yvals <- as.numeric(counts$V1)
  main_title <- sprintf("%s\nΔεν απάντησαν: %d Μέσος όρος: %.1f",get_full_question(question,full_questions),da_count,round(sum(xvals*yvals)/sum(yvals),1))
  if (any(count_200)) {
    main_title <- paste(main_title,'\nΔήλωσαν πως παρουσιάστηκε μεταγενέστερα,\nαλλά χωρίς να δηλώσουν ηλικία:',count_200)
  }
  theme_set(
    theme_gray() + 
      theme(text = element_text(size = 8)) + 
      theme(plot.title = element_text(size = 8)) + 
      theme(line = element_line(colour = "blue")) +
      theme(plot.margin = unit(c(0,0,0,0),'cm'))
    )
  count_plot <- qplot(
    xvals,
    yvals,
    main = main_title,
    ylab = 'Κατανομή',
    xlab = 'Ηλικία',
    margins = TRUE,
#     colour = I("blue"),
#     colour = "blue",
#     fill = "blue",
    geom="histogram",
    stat="identity",
    asp = 3/4
  )
  ggsave(
    filename = get_plotpath(out_folder,question),
    plot = count_plot,
    dpi = 300,
    width = 12,
    height = 9,
    units = "cm"
  )
}