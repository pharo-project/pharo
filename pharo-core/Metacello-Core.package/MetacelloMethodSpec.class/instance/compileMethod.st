method generation
compileMethod

	(project configuration class
		compile: self methodSource
		classified: self category) == nil
		ifTrue: [ self error: 'Error compiling the method' ]