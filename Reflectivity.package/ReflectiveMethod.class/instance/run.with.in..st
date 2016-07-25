evaluation
run: aSelector with: anArray in: aReceiver	
	"we install the old one as the compiler might need it"
	self installCompiledMethod.
	self compileAndInstallCompiledMethod.
	^aReceiver rFwithArgs: anArray executeMethod: compiledMethod