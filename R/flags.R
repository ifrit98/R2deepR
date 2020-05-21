

FLAGS <- tensorflow::flags(
  # Training
  flag_integer("epochs", 20),
  flag_integer("steps_per_epoch", 250),
  flag_integer("batch_size", 32),
  flag_integer("val_size", 100),
  flag_integer("patience", 40),
  flag_integer("valid_freq", 3),

  # Optimizer
  flag_string("optimizer", "adam"), # TODO: use optimizer$get(FLAGS$optimizer) to set params
  flag_numeric("init_lr", 0.001),
  flag_numeric("max_lr", 3),
  flag_numeric("momentum", 0),
  flag_numeric("decay", 0),
  flag_boolean("nesterov", FALSE),

  # Model
  flag_boolean("global_pool", TRUE),
  flag_boolean("batchnorm", FALSE),
  flag_integer("clipvalue", 0),
  flag_integer("clipnorm", 0),

  # Misc
  flag_boolean("profile", FALSE)
)
