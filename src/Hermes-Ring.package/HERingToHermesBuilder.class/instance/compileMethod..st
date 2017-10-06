compiling
compileMethod: aRFiMethod
	^ OpalCompiler new
		class: (HERingBinder onMethod: aRFiMethod);
		source: aRFiMethod sourceCode;
		compile