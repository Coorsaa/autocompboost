### Run with command:

### `R -d "valgrind --tool=callgrind --callgrind-out-file=valgrind-out/callgrind.out --log-file=valgrind-out/callgrind-log.txt" -f debug-compboost.R`
### Open and visualize with KCachegrind

### `Rscript debug-compboost.R -d "valgrind --tool=massif --stacks=yes --threshold=0 --detailed-freq=1 --time-unit=ms --verbose --trace-children=yes --massif-out-file=valgrind-out/massif.out --log-file=valgrind-out/massif-log.txt"`

devtools::load_all()
#remotes::install_github("schalkdaniel/compboost@tensors")
#devtools::load_all("~/repos/compboost")

lr_es = lrn("classif.compboost", stop_patience = 200L, stop_epsylon_for_break = 0, learning_rate_univariat = 0.1,
  learning_rate_interactions = 0.1, max_minutes_univariat = 2L, max_minutes_interaction = 2L, show_output = TRUE,
  predict_type = "prob", ntop_interaction = 10L, iters_max_univariat = 2000L,  iters_max_interactions = 2000L, use_early_stopping = TRUE, just_univariat = FALSE)
#lr_es$train(tsk("sonar"))

lr_nes = lrn("classif.compboost", stop_patience = 200L, stop_epsylon_for_break = 0, learning_rate_univariat = 0.1,
  learning_rate_interactions = 0.1, max_minutes_univariat = 2L, max_minutes_interaction = 2L, show_output = TRUE,
  predict_type = "prob", ntop_interaction = 10L, iters_max_univariat = 2000L,  iters_max_interactions = 2000L, use_early_stopping = FALSE, just_univariat = FALSE)
#lr_nes$train(tsk("sonar"))

mbr = microbenchmark::microbenchmark(lr_es$train(tsk("sonar")), lr_nes$train(tsk("sonar")), times = 10L)
mbr
