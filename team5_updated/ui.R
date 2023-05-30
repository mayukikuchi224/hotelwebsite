ui <- fluidPage(
  
  title = 'Hotel Columbia', # what shows on the browser tab

  theme = shinytheme('cerulean'), # theme from the shinythemes package
  
  # control which image shows on which page
  uiOutput('wallppr'),
  
  # create the tabs to show at the top
  uiOutput('uiTabs'),
  
  # create the ui output of all pages of website
  lapply(
    1:8,
    function(i) {
      uiOutput(paste0('ui', str_sub(tl[i], 1, 4)))
    }
  )
  
)
