compilation
compile
    "This method is the how compiling a method used to work.  All these steps were done at once.
     This method should not normally be used, because the whole point of MethodAddition is to let
	you first create a compiled method and then install the method later."
	self createCompiledMethod.
	self installMethod.
	self notifyObservers.
	^selector