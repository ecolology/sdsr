# Central limit theorem with spatial values:
sample_size <- 30
n_reps <- 10000
xbar <- c()
ybar <- c()

# Consider a domain between [0,10] for spatial long-lats x and y:
for (i in 1:n_reps){
	x <- runif(sample_size, 0, 10) # assume an original uniform pop distribution of [0,10] for x and y
	y <- runif(sample_size, 0, 10)
	xbar <- c(xbar, mean(x))
	ybar <- c(ybar, mean(y))
}
dat <- data.table(lon = xbar,
				  lat = ybar)
summary_dat <- dat %>% 
	summarise(lon = mean(lon),
			  lat = mean(lat))
dat %>% 
	ggplot(aes(lon, lat)) +
	geom_point(alpha = 0.1) + 
	geom_density_2d() +
	geom_point(data = summary_dat, color = "red")
