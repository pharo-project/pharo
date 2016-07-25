I am a refactoring operations for adding method arguments.

You can modify the method name and add an additional keyword argument and the default value used by senders of the original method.

This refactoring will add a new method with the new argument, remove the old method and replace every sender of the prior method with the new one, using the specified default argument.