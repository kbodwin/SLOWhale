#' Helper function for selection table
#'
#' @param df A data frame
#' @param name The name of the model
#' @param w The size of the time window, in seconds
#'
#' @importFrom dplyr filter group_by summarize mutate select row_number
#' @importFrom data.table frollmean rleid

to_selection_table <- function(df, name, w){
  df$total.pred <- 0
  possible_preds <- c("knn.pred.mel", "knn.pred.stft", "rf.pred.stft", "rf.pred.mel")
  for(pred in possible_preds) {
    if(pred %in% colnames(df)) {
      df$total.pred <- df$total.pred + as.numeric(levels(df[[pred]]))[df[[pred]]]
    }
  }

  predictions.v1 <- df |>
    # need to figure out how to only make it so the correct pred variables are used otherwise it will give error
    mutate(rolling.avg = frollmean(total.pred, n = w, fill = 0),
            final_pred = ifelse(rolling.avg > 0, 1, 0),
            pred_number= rleid(final_pred),
            `Begin Time (s)` = time_start, - (w/10),
            `End Time (s)` = time_end - (w/10)) #adjust timeframe proportional to n (3 is too far)

  selection.table <- predictions.v1 |>
      filter(final_pred == 1) |>
      group_by(pred_number) |>
      summarise( `Begin Time (s)`= min(`Begin Time (s)`),
                 `End Time (s)` = max(`End Time (s)`)) |>
      mutate(Selection = row_number(),
             View = "Spectrogram 1",
             Channel = 1) |>
      select(Selection, View, Channel, `Begin Time (s)`, `End Time (s)`)
    # write model predictions to .txt
    # this can be put straight into raven
    #nname <- sub(".wav","", name)
    #file_path <- file.path(path, paste0("/preds-",(w/10),"sec-",nname ,".txt"))


    return(selection.table)
}
