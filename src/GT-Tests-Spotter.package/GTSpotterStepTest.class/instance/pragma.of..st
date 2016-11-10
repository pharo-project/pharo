private-navigation
pragma: aSelector of: aCompiledMethod
	^ aCompiledMethod pragmas
		detect: [ :each | each selector = aSelector ]
		ifNone: [ nil ]