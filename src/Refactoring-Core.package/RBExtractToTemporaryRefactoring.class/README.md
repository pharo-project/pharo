I am a refactoring for extracting code fragments to a new temporary variable. 

You can select an interval of some code in a method and call this refactoring to create a new temporary variable, initialized with the value of this code.

My preconditions verify that the new temporary name is a valid name and isn't already used (neither a temporary, an instance variable or a class variable).