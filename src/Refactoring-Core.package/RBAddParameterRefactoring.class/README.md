I am a refactoring operations for adding method arguments.

You can modify the method name and add an additional keyword argument and the default value used by senders of the original method. Only one new argument can be added. But you can change the whole method name, as long as the number of argument matches.

For example, for #r:g:b:  add another parameter "a" the new method is
#r:g:b:a: 
or change the whole method to 
#setRed:green:blue:alpha:

This refactoring will add a new method with the new argument, remove the old method (for all implementors) and replace every sender of the prior method with the new one, using the specified default argument.