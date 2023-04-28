library(bayeslm)
library(dplyr)

### initialize the simulation
init_agent <- function(population, id, NUM_AGENTS, MAX_FRIENDS, MAX_CHILDREN) {
    # generate values for each attribute
    age <- round(runif(1, min = 20, max = 60))
    health_score <- rnorm(1, mean = 50, sd = 20)
    num_children <- rbinom(1, MAX_CHILDREN, 0.2)
    num_friends <- rbinom(1, MAX_FRIENDS, 0.5)
    avg_health <- 0
    # add to dataframe
    population <- rbind(population, 
                        data.frame(Id = id,
                                Age = age,
                                HealthScore = health_score,
                                NumChildren = num_children,
                                AveHealth = avg_health,
                                NumFriends = num_friends)
                    )
    return(population)
}

init_population <- function(NUM_AGENTS, MAX_FRIENDS, MAX_CHILDREN) {
    # attritbutes for each agent
    columns <- c("Id", "Age", "HealthScore",
                "NumChildren", "AveHealth",
                "NumFriends")
    # init the dataframe
    population <- data.frame(matrix(ncol = length(columns), nrow = 0))
    colnames(population) <- columns
    # fill in values for each agent
    for (id in 1:NUM_AGENTS) {
        population <- init_agent(population, id, 
                                NUM_AGENTS, MAX_FRIENDS, MAX_CHILDREN)
    }
    return(population)
}

init_tie_types <- function() {
    friend_types <- c("Kin", "Neighbor", "Coworker")
    friend_coefs <- c(2, 1.5, 0.8)
    tie_df <- data.frame(matrix(ncol = length(friend_types)))
    colnames(tie_df) <- friend_types
    tie_df[1, ] <- friend_coefs

    return(tie_df)
}

generate_ties <- function(population, MAX_FRIENDS, tie_df) {
    # add column for each potential friend
    columns <- c()
    for (i in 1: MAX_FRIENDS) {
        columns[length(columns) + 1] <- paste0("Friend", i)
        columns[length(columns) + 1] <- paste0("Friend", i, "_Type")
    }
    # create parallel friend dataframe
    friends <- data.frame(matrix(ncol = MAX_FRIENDS, nrow = 0))
    for (row in seq_len(nrow(population))) {
        # make list of friend ids
        num_friends <- population[row, "NumFriends"]
        friend_ids <- sample.int(nrow(population), num_friends)
        friend_list <- as.list(rep(NA, MAX_FRIENDS * 2))
        sum_friend_health <- 0
        # fill in list with id, tie type, and calculate mean health score
        for (f in seq_along(friend_ids)) {
            friend <- friend_ids[f]
            friend_list[2 * f - 1] <- friend
            friend_list[2 * f] <- sample(colnames(tie_df), 1)
            friend_health <- population[friend, "HealthScore"]
            sum_friend_health <- sum_friend_health + friend_health
        }
        # add to dataframes
        friends <- rbind(friends, friend_list)
        population[row, "AveHealth"] <- sum_friend_health / num_friends
    }
    colnames(friends) <- columns
    # merge dataframes
    population <- merge(population, friends, by.x = "Id", by.y = 0)

    return(population)
}

find_space <- function(world) {
    x <- sample.int(dim(world)[1], 1)
    y <- sample.int(dim(world)[2], 1)
    cell <- c(x, y)
    if (!is.na(world[x, y])) {
        cell <- find_space(world)
    }

    return(cell)
}

insert_object <- function(world, object, df, DIM, i) {
    cell <- find_space(world)
    world[cell[1], cell[2]] <- object
    for (d in 1:DIM) {
        df[i, d] <- cell[d]
    }

    return(list(world = world, df = df))
}

init_world <- function(GRID_LENGTH, GRID_WIDTH, population) {
    DIM <- 2
    NUM_PARKS <- 10
    NUM_HOSPITALS <- 10
    PARK_BUFF <- 0.3
    HOSPITAL_BUFF <- 0.3
    # init world and dataframe of building locations
    world <- data.frame(matrix(nrow = GRID_LENGTH, ncol = GRID_WIDTH))
    buildings <- data.frame(matrix(nrow = NUM_PARKS + NUM_HOSPITALS, 
                                    ncol = DIM + 3))
    columns <- c()
    for (d in 1:DIM) {
        columns[length(columns) + 1] <- paste0("Dim", d)
    }
    colnames(world) <- 1:GRID_WIDTH
    colnames(buildings) <- c(columns, "Id", "Building", "Buff")
    # insert parks
    for (i in 1:NUM_PARKS) {
        world_and_building <- insert_object(world, paste0("Park_", i),
                                            buildings, DIM, i)
        world <- world_and_building$world
        buildings <- world_and_building$df
        buildings$Id[i] <- i
        buildings$Building[i] <- "Park"
        buildings$Buff[i] <- PARK_BUFF
    }
    # insert hospitals
    for (i in NUM_PARKS + 1:NUM_HOSPITALS) {
        world_and_building <- insert_object(world, paste0("Hospital_", i), 
                                            buildings, DIM, i)
        world <- world_and_building$world
        buildings <- world_and_building$df
        buildings$Id[i] <- i
        buildings$Building[i] <- "Hospital"
        buildings$Buff[i] <- HOSPITAL_BUFF
    }
    # insert people
    locations <- data.frame(matrix(nrow = nrow(population), 
                                    ncol = DIM))
    colnames(locations) <- columns
    for (id in 1:nrow(population)) {
        world_and_building <- insert_object(world, paste0("Agent_", id), 
                                            locations, DIM, id)
        world <- world_and_building$world
        locations <- world_and_building$df
    }
    # merge locations into population dataframe
    population <- merge(population, locations, by.x = "Id", by.y = 0)
 
    return(list(world = world, population = population, buildings = buildings))
}

euclidean <- function(p1, p2) sqrt(sum((p1 - p2)^2))

# will probably break at K > 1
k_nearest <- function(population, buildings, K) {
    # get locations of agents and buildings
    start_i <- which(colnames(population) == "Dim1")
    cols <- c(start_i, start_i + 1)
    agent_locs <- population[, cols]

    park_df <- buildings %>% filter(Building == "Park")
    hosp_df <- buildings %>% filter(Building == "Hospital")
    start_i <- which(colnames(buildings) == "Dim1")
    cols <- c(start_i, start_i + 1)
    park_locs <- park_df[, cols]
    hosp_locs <- hosp_df[, cols]

    assigned <- data.frame(matrix(ncol = 6, nrow = 0))
    for (row in seq(nrow(population))) {
        # get distances from agent to each building type
        agent_loc <- agent_locs[row, ]
        park_dists <- c()
        for (i in seq(nrow(park_locs))) {
            park_loc <- park_locs[i, ]
            park_dists[length(park_dists) + 1] <- euclidean(agent_loc, park_loc)
        }
        hosp_dists <- c()
        for (i in seq(nrow(hosp_locs))) {
            hosp_loc <- hosp_locs[i, ]
            hosp_dists[length(hosp_dists) + 1] <- euclidean(agent_loc, hosp_loc)
        }
        # get k lowest distances
        k_parks_idx <- sort(park_dists, index.return = TRUE)$ix[1:K]
        k_hosps_idx <- sort(hosp_dists, index.return = TRUE)$ix[1:K]
        k_parks_buff <- buildings[k_parks_idx, "Buff"]
        k_hosps_buff <- buildings[k_hosps_idx, "Buff"]

        # add id of assigned building
        # (added number of park_locs to line up matched ids with building ids)
        assigned_row <- c(k_parks_idx, park_dists[k_parks_idx], k_parks_buff,
                        k_hosps_idx + nrow(park_locs), hosp_dists[k_hosps_idx],
                        k_hosps_buff)
        assigned <- rbind(assigned, assigned_row)
    }
    # append to pop df
    colnames(assigned) <- c("ParkId", "ParkDist", "ParkBuff", 
                            "HospitalId", "HospitalDist", "HospitalBuff")
    population <- merge(population, assigned, by.x = "Id", by.y = 0)

    return(population)
}

### calculate next time step
calc_ave_health <- function(population, tie_df, MAX_FRIENDS) {
    # modify AveHealth by taking weighted sum of friend health
    start <- which(colnames(population) == "Friend1")
    end <- which(colnames(population) == paste0("Friend", MAX_FRIENDS, "_Type"))
    friend_data <- population[, start:end]

    for (n in 1:nrow(population)) {
        sum_health <- 0
        num_friends <- population[n, "NumFriends"]

        if (num_friends == 0) {
            population[n, "AveHealth"] <- population[n, "HealthScore"]
        }
        else {
            for (f in 1:num_friends) {
                friend_id <- friend_data[n, 2 * f - 1]
                if (is.na(friend_id)) {
                    break
                }
                else {
                    tie_coef <- tie_df[, friend_data[n, 2 * f]]
                    friend_health <- population[friend_id, "HealthScore"]
                    sum_health <- sum_health + tie_coef * friend_health
                }
            }
            population[n, "AveHealth"] <- sum_health / num_friends
        }
    }

    return(population)
}

next_health_score <- function(population, buildings) {
    # coefs
    age_coef <- 0.33
    curr_coef <- 0.33
    friend_coef <- 0.33
    next_coef <- 0.9
    # vectors
    next_noise <- rnorm(nrow(population), mean = 0, sd = 5)
    friend_noise <- rnorm(nrow(population), mean = 0, sd = 5)

    health_score <- population[, "HealthScore"]
    age <- population[, "Age"]
    ave_health <- population[, "AveHealth"]

    park_effect <- population[, "ParkBuff"] * 1 / population[, "ParkDist"]
    hosp_effect <- population[, "HospitalBuff"] * 1 / population[, "HospitalDist"] # nolint
    # calculate
    next_health <- age_coef * age + 
                    curr_coef * health_score + 
                    friend_coef * ave_health +
                    park_effect + hosp_effect +
                    next_noise
    friend_health <- next_coef * next_health +
                    friend_noise
    # add to df
    population$HealthScore <- next_health
    population$AveHealth <- friend_health

    return(population)
}

make_scatter <- function(t, population) {
    png(paste0("plots/plot_", t, ".png"))
    plot(population[, "HealthScore"], population[, "AveHealth"], 
        main = paste0("Plot ", t, ": Ego vs Averaged Alter Health"),
        xlab = "Ego Health Score", ylab = "Averaged Alter Health Score",
        xlim = c(0, 100), ylim = c(0, 100))
    dev.off()

    return()
}

main <- function() {
    # hyperparams for population init
    NUM_AGENTS <- 10
    MAX_FRIENDS <- 3
    MAX_CHILDREN <- 10
    # init population of agents
    population <- init_population(NUM_AGENTS, MAX_FRIENDS, MAX_CHILDREN)
    # init tie types between agents
    tie_df <- init_tie_types()
    # generate friendships
    population <- generate_ties(population, MAX_FRIENDS, tie_df)
    # init grid world with buildings and agents
    GRID_LENGTH <- 10
    GRID_WIDTH <- 10
    world_and_pop <- init_world(GRID_LENGTH, GRID_WIDTH, population)
    world <- world_and_pop$world
    population <- world_and_pop$population
    buildings <- world_and_pop$buildings
    # find nearest of each building type for each agent
    population <- k_nearest(population, buildings, K = 1)
    # run simulation
    TIME_STEPS <- 100
    make_scatter(1, population)

    for (t in 1:TIME_STEPS) {
        population <- calc_ave_health(population, tie_df, MAX_FRIENDS)
        population <- next_health_score(population, buildings)
        if (t %% 10 == 0) {
            make_scatter(t, population)
        }
    }

    return()
}

main()