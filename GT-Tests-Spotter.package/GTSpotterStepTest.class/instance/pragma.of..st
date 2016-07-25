private-navigation
pragma: aSelector of: aCompiledMethod
	^ aCompiledMethod pragmas 
		detect: [ :each | each keyword = aSelector ] 
		ifNone: [ nil ]