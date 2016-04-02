# bindings-wlc

WIP bindings against libwlc

## Using Bindings.WLC
Low level bindings to WLC via bindings-dsl

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

## Using System.WLC
System.WLC builds on Binding.WLC

```haskell
do
  initialize
  logHandler (\tag text -> putStrLn $ stringTag tag ++ " " ++ text) -- Log all the things
  dispatchEvent (PointerMotion (\_ _ ptr -> do pointerSetPosition ptr; return True)) -- Allow mouse movement
  dispatchEvent (ViewCreated (\view -> do viewBringToFront view; viewFocus view; return True)) -- Pop views to the front
  dispatchEvent (CompositorReady $ exec "weston-terminal" []) -- Launch weston-terminal when we're ready
  run
```
