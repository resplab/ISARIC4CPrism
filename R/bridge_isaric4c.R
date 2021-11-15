#' Title
#'
#' @param model_input
#'
#' @return
#' @export
#'
#' @examples
model_run<-function(model_input = NULL)
{

  input<-unflatten_list(model_input)

  if (model_input$func == 0) {
    results <- isaric4c        (age               =model_input$age,
                                sex               =model_input$sex,
                                num_comorbidities =model_input$num_comorbidities,
                                respiratory_rate  =model_input$respiratory_rate,
                                admission_oxygen_saturation   =model_input$admission_oxygen_saturation,
                                glasgow_coma_scale=model_input$glasgow_coma_scale,
                                urea              =model_input$urea,
                                crp               =model_input$crp,
                                func              =model_input$func)
  } else {
    results <- isaric4c        (age               =model_input$age,
                                sex               =model_input$sex,
                                respiratory_rate  =model_input$respiratory_rate,
                                admission_oxygen_saturation   =model_input$admission_oxygen_saturation,
                                glasgow_coma_scale=model_input$glasgow_coma_scale,
                                urea              =model_input$urea,
                                crp               =model_input$crp,
                                nosocomial        =model_input$nosocomial,
                                radiographic_chest_infiltrates=model_input$radiographic_chest_infiltrates,
                                receiving_oxygen  =model_input$receiving_oxygen,
                                lymphocytes       =model_input$lymphocytes,
                                func              =model_input$func)

  }


  return(as.list(results))
}




#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}
