I am a refactoring operation for renaming methods.

The new method name has to have the same number of arguments, but the order of arguments can be changed.

My precondition verifies the number of arguments and if the new method name isn't already used. But 
you can use the same method name and just permutate the arguments.

All references in senders of the old method are changed, either the method name only or the order of the supplied 
arguments too.
