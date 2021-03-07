# mcpd-racket
Simulation of a microchip pet door in Racket with external DSL *because I can*, although I'm a Racket beginner and the code shows - my future self will hopefully fix this code with elegant idiomatic Racket, or rewrite this project from scratch, or delete it and build something else entirely instead. Yet again, he can use this project to learn how to make a Racket DSL instead of an external one.

![This image is not mine](https://www.dougsartgallery.com/images/xASCII-cat-kitten-1-mmmm.gif.pagespeed.ic.hdGGZj0vgJ.webp)

To test this code you need Racket installed (I tested it with versions 7.8 and 7.9) and a unix/linux shell of any type. To run the application open a shell and run:
````shell
racket main.rkt
````
It opens an interactive prompt. Since I'm lazy and I have already provided a help command (which you run by typing ``help`` and pressing enter), I'll copy its output here below.

~~~~
- curfew: Checks if the curfew is active.
  Usage:> curfew

- enter: Asks if the cat can enter.
  Usage:> enter [id]

- leave: Asks if the cat can leave.
  Usage:> leave [id]

- help: Prints the list of available commands.
  Usage:> help

- mode: Displays the active mode.
  Usage:> mode

- register: Adds a cat to the registry.
  Usage:> register [id]*

- registered: Checks if a cat is registered.
  Usage:> registered [id]*

- reset-registry: Resets the registry.
  Usage:> reset-registry

- switch-mode: Switches to the next mode.
  Usage:> switch-mode

- toggle-curfew: Activates/deactivates the special curfew mode.
  Usage:> toggle-curfew

- unregister: Removes a cat from the registry.
  Usage:> unregister [id]*
~~~~

A little example: try to type
~~~~
enter Tzatziki
~~~~
and
~~~~
leave Tzatziki
~~~~
and observe the output. You ask the pet door the permission to let Tzatziki enter and leave (but Tzatziki isn't registered in the system yet).
Now we register Tzatziki by entering:
~~~~
register Tzatziki
~~~~
Now try again to type
~~~~
enter Tzatziki
~~~~
and
~~~~
leave Tzatziki
~~~~
Most likely you get different results. Check [rules.mcpd](src/rules.mcpd) to see how the pet door is set up.
