helper
compile: source
	^OpalCompiler new 
		class: self class; 
		source: source; 
		compile