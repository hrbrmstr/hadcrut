#+ message=FALSE
library(readr)    # read_table() / write_csv()
library(dplyr)
library(zoo)      # as.yearmon()
library(ggplot2)  # devtools::install_github("hadley/ggplot2")
library(hrbrmisc) # devtools::install_github("hrbrmstr/hrbrmisc")
library(viridis)

#' TODO: Monitor <http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html> for changes

URL <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.monthly_ns_avg.txt"
fil <- sprintf("data/%s", basename(URL))
if (!file.exists(fil)) download.file(URL, fil)

global_temps <- read_table(fil, col_names=FALSE)

global_temps %>%
  select(year_mon=1, median=2, lower=11, upper=12) %>%
  mutate(year_mon=as.Date(as.yearmon(year_mon, format="%Y/%m")),
         year=as.numeric(format(year_mon, "%Y")),
         decade=(year %/% 10) * 10,
         month=format(year_mon, "%b")) %>%
  mutate(month=factor(month, levels=month.abb)) %>%
  filter(year != 2016) -> global_temps

# for D3 vis
write_csv(global_temps, "data/temps.csv")

#+ hadcrut, fig.retina=2, fig.width=12, fig.height=6
gg <- ggplot(global_temps)
gg <- gg + geom_segment(aes(x=year_mon, xend=year_mon, y=lower, yend=upper, color=year), size=0.2)
gg <- gg + geom_point(aes(x=year_mon, y=median), color="white", shape=".", size=0.01)
gg <- gg + scale_x_date(name="Median in white", expand=c(0,0.5))
gg <- gg + scale_y_continuous(name=NULL, breaks=c(0, 1.5, 2),
                              labels=c("0°C", "1.5°C", "2.0°C"), limits=c(-1.6, 2.25))
gg <- gg + scale_color_viridis(option="C")
gg <- gg + facet_wrap(~decade, nrow=1, scales="free_x")
gg <- gg + labs(title="Global Temperature Change (1850-2016)",
                subtitle="Using lower and upper bounds of the 95% confidence interval of the combined effects of all the uncertainties described in the HadCRUT4 error model\n(measurement and sampling, bias and coverage uncertainties; fields 11 & 12)",
                caption="HadCRUT4 [4.5] (http://www.metoffice.gov.uk/hadobs/hadcrut4/index.html)")
gg <- gg + theme_hrbrmstr(grid="XY")
gg <- gg + theme(panel.background=element_rect(fill="black", color="#2b2b2b", size=0.15))
gg <- gg + theme(panel.spacing.x=unit(0, "null"))
gg <- gg + theme(panel.grid.major.y=element_line(color="#b2182b", size=0.25))
gg <- gg + theme(strip.text=element_text(hjust=0.5))
gg <- gg + theme(axis.title.x=element_text(hjust=0, margin=margin(t=-10)))
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(axis.text.y=element_text(size=12, color="#b2182b"))
gg <- gg + theme(plot.margin=margin(10, 10, 10, 10))
gg <- gg + theme(legend.position="none")
gg
