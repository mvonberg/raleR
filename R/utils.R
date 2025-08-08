# Remove path data and file extension from audio file name (used as audio tag names that playback elements (buttons/sliders) can refer to)
get_filename_from_path <- function(src) {
  tools::file_path_sans_ext(
    unlist(strsplit(src,split="/"))[lengths(strsplit(src,split="/"))])
}

# Create hidden audio tag with id that can be found by playback elements
hidden_audio <- function(src,id=get_filename_from_path(src)) {
  shiny::tags$audio(id=id,
                    shiny::tags$source(id=paste0(id,"Source"),src=src,type=paste0("audio/", tools::file_ext(src))))
}

# array of audio file URLs for ABX tests
audioURLs_ABX <- function(url_dir,source,N_source,idx,file_type) {
  data.frame(matrix(paste0(url_dir,source,1:N_source,"_",sprintf("%02d",rep(idx,each=N_source)),".",file_type),
                    nrow=3,dimnames=list(NULL,paste0("stim",idx))
                    )
             )
}

# Action button extension that creates a hidden audio component and calls the JS \code{toggleSounds} function for playback management.
audio_button <- function(src_audio,label,btn.size,id=sapply(src_audio,get_filename_from_path)) {
  out <- shiny::div(Map(shiny::actionButton,inputId=paste0("btn",id),
                        label = label,
                        width = btn.size,
                        onclick = paste0("toggleSounds","('",id,"')")),
                    Map(hidden_audio,src=src_audio,id=id))
  return(out)
}


# Create a vector of file links with increasing numbers at the end
file_link_array <- function(base,min=NULL,max=NULL,range=min:max,file_ext="mp3",format_numbers) {
  paste0(base,sprintf(format_numbers,range),".",file_ext)
  }

# Sample a range of consecutive values from a vector that include a target
sample_range <- function(target,vector_in,range_length,target_margin=0,exclude_target_at_center=TRUE) {
  # make sure target is a number
  if (!is.numeric(target)) {
    target <- match(target,vector_in)
  }
  # make sure target will be is within the sampled range
  max_offset <- min(c(length(vector_in)-range_length,
                     target-(target_margin+1)))
  min_offset <- max(c(0,
                      target-(range_length-(target_margin+1))))
  offset_range <- min_offset:max_offset
  if (exclude_target_at_center) {
    offset_target_at_center <- ((target-target_margin):(target+target_margin)) - ceiling(range_length/2)
    offet_range <- offset_range[is.na(match(offset_range,offset_target_at_center))]
  }
  offset <- sample(offset_range,1) # random offset within possible range
  sampled_range <- vector_in[offset+(1:range_length)]
  return(sampled_range)
}

# Sample a range of values from a vector, excluding values with margins around a specific value
constrained_sample <- function(range,N=1,margin=0) {
  range_constrained <- range[(margin+1):(length(range)-margin)] # cut margins
  out <- sample(range_constrained,N) # sample N values from the constrained range
  return(out)
}

# feedback plot (copy of the TPT feedback plot)
feedback_plot <- function(score){
  p <- plotly::plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = score,
    title = list(text = psychTestR::i18n("FEEDBACK_PLOT")),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      bar = list(color = "#8cc77f"),
      axis =list(range = list(0, 100)),
      steps = list(
        list(range = c(0, 20), color = "#ffe4b3"),
        list(range = c(20, 40), color = "#ffd58a"),
        list(range = c(40, 60), color = "#ffc65e"),
        list(range = c(60, 80), color = "#ffb836"),
        list(range = c(80, 100), color = "#ffa500"))
    ),
    height = 400,
    width = 500)

  plotly::layout(p, margin = list(l=20,r=30))
}
