library(hubAdmin)

# Function
make_task_id <- function(origin_date, scenario, req_target, max_horizon,
                         opt_target = NULL, location = NULL, age_group = NULL) {
  if (is.null(location)) {
    location <- c("US", "01", "02", "04", "05", "06", "08", "09", "10", "11",
                  "12", "13", "15", "16", "17", "18", "19", "20", "21", "22",
                  "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
                  "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
                  "44", "45", "46", "47", "48", "49", "50", "51", "53", "54",
                  "55", "56", "60", "66", "69", "72", "74", "78")
  }

  or_task <- create_task_id("origin_date",
                            required = origin_date,
                            optional = NULL,
                            schema_version = "v3.0.0",
                            branch = "br-v3.0.0")
  scen_task <- create_task_id("scenario_id",
                              required = scenario,
                              optional = NULL,
                              schema_version = "v3.0.0",
                              branch = "br-v3.0.0")
  loc_task <- create_task_id("location",
                             required = NULL,
                             optional = location,
                             schema_version = "v3.0.0",
                             branch = "br-v3.0.0")
  targ_task <- create_task_id("target",
                              required = req_target,
                              optional = opt_target,
                              schema_version = "v3.0.0",
                              branch = "br-v3.0.0")
  hor_task <- create_task_id("horizon",
                             required = as.integer(seq(1, max_horizon,
                                                       by = 1)),
                             optional = NULL,
                             schema_version = "v3.0.0",
                             branch = "br-v3.0.0")
  arg_list <- list(or_task, scen_task, loc_task, targ_task, hor_task)

  if (!is.null(age_group)) {
    age_task <- create_task_id("age_group",
                               required = age_group,
                               optional = NULL,
                               schema_version = "v3.0.0",
                               branch = "br-v3.0.0")
    arg_list <- c(arg_list, list(age_task))
  }

  task_ids <- do.call(create_task_ids, arg_list)
  return(task_ids)
}

# target metadata -----
inchosp_meta <-
  create_target_metadata_item(target_id = "inc hosp",
                              target_name = "Incident Hospitalization",
                              description = "Weekly newly reported hospitalizations where the patient has COVID-19, as reported by hospital facilities and aggregated in the HHS Protect data collection system.",
                              target_units = "count",
                              target_keys = list(target = "inc hosp"),
                              target_type = "discrete",
                              is_step_ahead = TRUE,
                              time_unit = "week",
                              schema_version = "v3.0.0",
                              branch = "br-v3.0.0")
cumhosp_meta <-
  create_target_metadata_item(target_id = "cum hosp",
                              target_name = "Cumulative Hospitalization",
                              description = "Weekly cumulative reported hospitalizations where the patient has COVID-19, since the beginning of the simulation.",
                              target_units = "count",
                              target_keys =  list(target = "cum hosp"),
                              target_type = "discrete",
                              is_step_ahead = TRUE,
                              time_unit = "week",
                              schema_version = "v3.0.0",
                              branch = "br-v3.0.0")
incdeat_meta <-
  create_target_metadata_item(target_id = "inc death",
                              target_name = "Incident Death",
                              description = "Weekly new deaths due to COVID-19 as reported by state and local departments of public health and aggregated in the NCHS data collection system.",
                              target_units = "count",
                              target_keys = list(target = "inc death"),
                              target_type = "discrete",
                              is_step_ahead = TRUE,
                              time_unit = "week",
                              schema_version = "v3.0.0",
                              branch = "br-v3.0.0")
cumdeat_meta <-
  create_target_metadata_item(target_id = "cum death",
                              target_name = "Cumulative Death",
                              description = "Weekly cumulative deaths due to COVID-19 since the beginning of the simulation.",
                              target_units = "count",
                              target_keys = list(target = "cum death"),
                              target_type = "discrete",
                              is_step_ahead = TRUE,
                              time_unit = "week",
                              schema_version = "v3.0.0",
                              branch = "br-v3.0.0")

# Round 2024-04-28 -----

# Sample
task_smp_20240428 <- make_task_id(origin_date = "2024-04-28",
                                  scenario = c("A-2024-03-01", "B-2024-03-01",
                                               "C-2024-03-01", "D-2024-03-01",
                                               "E-2024-03-01", "F-2024-03-01"),
                                  req_target = c("inc death", "inc hosp"),
                                  max_horizon = 52,
                                  opt_target = c("cum death", "cum hosp"),
                                  location = NULL,
                                  age_group = c("0-130", "65-130", "0-64"))
cm_set <- c("origin_date", "scenario_id", "location", "target")
smp_type_20240428 <-
  create_output_type(create_output_type_sample(is_required = TRUE,
                                               output_type_id_type = "integer",
                                               min_samples_per_task = 100L,
                                               max_samples_per_task = 300L,
                                               compound_taskid_set = cm_set,
                                               value_type = "double",
                                               value_minimum = 0,
                                               schema_version = "v3.0.0",
                                               branch = "br-v3.0.0"))

# Quantiles
task_qua_20240428 <- make_task_id(origin_date = "2024-04-28",
                                  scenario = c("A-2024-03-01", "B-2024-03-01",
                                               "C-2024-03-01", "D-2024-03-01",
                                               "E-2024-03-01", "F-2024-03-01"),
                                  req_target = NULL,
                                  max_horizon = 52,
                                  opt_target = c("inc death", "inc hosp",
                                                 "cum death", "cum hosp"),
                                  location = NULL,
                                  age_group = c("0-130", "65-130", "0-64"))
quant <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5,
           0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
qua_type_20240428 <-
  create_output_type(create_output_type_quantile(required = quant,
                                                 optional = c(0, 1),
                                                 value_type = "double",
                                                 value_minimum = 0,
                                                 schema_version = "v3.0.0",
                                                 branch = "br-v3.0.0"))
# Metadata
t_meta_20240428 <- create_target_metadata(inchosp_meta, incdeat_meta,
                                          cumdeat_meta, cumhosp_meta)

# Model tasks ---
mod_20240428 <-
  create_model_tasks(create_model_task(task_smp_20240428,
                                       output_type = smp_type_20240428,
                                       target_metadata = t_meta_20240428),
                     create_model_task(task_qua_20240428,
                                       output_type = qua_type_20240428,
                                       target_metadata = t_meta_20240428))

# Round ---
round_20240428 <- create_round(round_id_from_variable = TRUE,
                               round_id = "2024-04-28",
                               model_tasks = mod_20240428,
                               submissions_due = list(start = "2024-04-01",
                                                      end = "2025-04-01"),
                               file_format = "parquet")

# Create tasks.json -----
tasks_config <- create_config(create_rounds(round_20240428))

