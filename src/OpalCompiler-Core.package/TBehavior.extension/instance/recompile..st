*OpalCompiler-Core
recompile: selector
	"Compile the method associated with selector in the receiver's method dictionary."
	^self recompile: selector from: self