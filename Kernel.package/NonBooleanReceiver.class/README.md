Some constructs are optimized in the compiler :
#whileTrue:
#whileFalse:
#ifTrue:
#ifFalse:
#ifTrue:ifFalse:
#ifFalse:ifTrue:
So you cannot by default use them on non boolean objects.
	
If you really need to use optimized constructs, you can enable Opal compiler and do one of the following :
		- recompile your method with the pragma : <compilerOptions: #(+ optIlineNone)>
		- recompile your class with the method : MyClass class>>compiler 
			^ super compiler options: #(+ optIlineNone)
		- call from this method by Object>>#mustBeBooleanInMagic:"