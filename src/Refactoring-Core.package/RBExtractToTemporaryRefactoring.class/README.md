Add a new temporary variable for the value of the selected code. Every place in this method using the same piece of code is replaced by accessing this new temporary variable instead.
As the code is now only evaluated once for initializing the variable value, this refactoring may modify the behavior if the code statements didn't evaluate to the same value on every call.

My preconditions verify that the new temporary name is a valid name and isn't already used (neither a temporary, an instance variable or a class variable).