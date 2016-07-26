private
classNamed: aClassName do: aBlock
	^ self classNamed: aClassName do: aBlock ifNone: [ 
		(GTExampleDeclaredClassMissing new
			example: self;
			classNamed: aClassName;
			yourself) signal ]