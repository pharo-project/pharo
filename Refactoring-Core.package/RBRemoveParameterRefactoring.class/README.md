I am a refactoring for removing (unused) arguments.

My precondition verify, that the argument to be removed, is not referenced by the method that will be renamed.
And that the new method name isn't alread used.
Any sender of the prior selector will be changed to the new.

If the method contains multiple keyword arguments, I request a dialog for selecting one of the argument.