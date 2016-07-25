accessing
compilationContext
	^ compilationContext ifNil: [ 
		"only happens when decompiling or using stand-alone"
		compilationContext := CompilationContext default]