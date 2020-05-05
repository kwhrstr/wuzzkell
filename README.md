# wuzzkell

This is simple [Wuzz](https://github.com/asciimoo/wuzz) implement from Haskell


![wuzzkell screencast](docs/images/screencast.gif)



## Build and usage

```
$ stack build
$ stack exec wuzzkell-exe
```

### Commands

Keybinding                              | Description
----------------------------------------|---------------------------------------
<kbd>Ctrl+R</kbd>, <kbd>enter</kbd>     | Send request
<kbd>esc</kbd>                          | Quit
<kbd>Tab</kbd>                          | Next view
<kbd>Shift+Tab</kbd>                    | Previous view
<kbd>Down</kbd>                         | Move down one view line
<kbd>Up</kbd>                           | Move up one view line
<kbd>Page down</kbd>                    | Move down one view page
<kbd>Page up</kbd>                      | Move up one view page

Mouse                                   | Description
----------------------------------------|---------------------------------------
Mouse Down                              | Jump to Select View 


## TODO

* configuration key-conf
* Better navigation
* Autocompletion
* Tests
