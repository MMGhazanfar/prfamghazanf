#Homework Function goes here
## QUESTION ----

#' Mustafa Ghazanfar
#'
#' \code{crime_correlate} This is a function designed to take to input from a crime data sheet,
#'		an offence description and two suburbs representing two suburbs. Then it will analyze
#'		and return a plot to show the number of incidents between the the suburbs for that year
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of one of the level 3 Offence Description.
#' @param suburbs A two-element character vector. Each element is an SA suburb.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples
#' <one or two examples showing how to use the function>
crime_correlate <-
  function(crime_data,
           offence_description,
           suburbs) {
    require(data.table)
    require(ggplot2)

    # Error catching
    print('test1')
    if (!all.equal(2, length(suburbs)) |
        ((suburbs[1]) == (suburbs[2]))) {
      stop("Please enter two different Adelaide suburbs")
    }
    print('test1.5')
    expected_colnames <-
      c(
        "date",
        "suburb",
        "postcode",
        "offence_level_1",
        "offence_level_2",
        "offence_level_3",
        "offence_count"
      )

    if (!all.equal(expected_colnames, colnames(crime_data))) {
      stop(paste(
        "Input table columns need to match: ",
        paste(expected_colnames, collapse = ", ")
      ))
    }
    print('test2')
    # Check that the input suburbs and offence description exist in crime_data
    if (any(!suburbs %in% crime_data$suburb) |
        !offence_description %in% crime_data$offence_level_3) {
      stop("Please verify that suburbs and offence  level 3 description match the crime data files")
    }

    # Make a data table for plotting using data.table transformations
    # You will need to filter, summarise and group by
    ## Expect cols: "date", "suburb", "total_offence_count"
    plot_data <-
      crime_data[suburb %in% suburbs &
                   offence_level_3 == offence_description, list(total_offence_count = sum(offence_count)), by = list(suburb, date)]

    #Code To set Title
    st_y <-
      format(as.Date(min(plot_data$date), format = "%d/%m/%Y"), "%Y")
    en_y <-
      format(as.Date(max(plot_data$date), format = "%d/%m/%Y"), "%Y")
    chart_title <- paste('Data for', st_y, '-', en_y)

    ## Generate the plot
    ggplot(plot_data, aes(x = date,
                          y = total_offence_count,
                          color = suburb)) +
      geom_line() + geom_smooth(method = glm) +
      labs(x = 'Date', y = 'Total Offences Commited', title = chart_title) +
      theme(
        plot.title = element_text(size = rel(2)),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_line(color = "grey95"),
        panel.grid.major.y = element_line(color = "grey95"),
        panel.grid.minor.x = element_line(color = "grey95"),
        panel.grid.major.x = element_line(color = "grey95"),
        strip.background = element_rect(colour = "black", fill = "Grey90")
      )
  }
