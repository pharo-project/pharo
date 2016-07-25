I model Class Variables and therefore Pool Variables, too. I am stored as a binding in the classPool of the class defining me.

The compiler forwards bytecode generation to me for accessing the variable.

You can subclass me and implement #read and #write: for creating special kinds of globals that can be used as special class Variables (similar to special Slots).
