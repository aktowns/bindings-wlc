# bindings-wlc

WIP bindings against libwlc

```haskell
do
  logger <- mk'log_handler_cb (\_ -> print)
  c'wlc_log_set_handler logger

  c'wlc_init2

  created_cb <- mk'output_created_cb (\output -> do
    putStrLn "Output created"
    return True)
  c'wlc_set_output_created_cb created_cb

  c'wlc_run
```
