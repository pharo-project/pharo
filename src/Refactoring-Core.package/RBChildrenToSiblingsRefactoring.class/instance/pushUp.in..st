private-methods
pushUp: aSelector in: aClass 
	| source |
	source := aClass sourceCodeFor: aSelector.
	source isNil 
		ifFalse: 
			[aClass superclass compile: source
				classified: (aClass protocolsFor: aSelector)]