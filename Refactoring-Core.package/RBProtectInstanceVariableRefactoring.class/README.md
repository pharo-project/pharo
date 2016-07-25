I am a refactoring for protecting instance variable access.

If a class defines methods for reading and writing instance variables, they are removed and all calls on this methods.
Omit method that are redefined in subclasses.