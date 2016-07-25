testing
testMethodsWithUnboundGlobals
	| methodsWithUnboundGlobals |
	"Ensure the environment is clean"
	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	Smalltalk cleanOutUndeclared. 
	methodsWithUnboundGlobals := SystemNavigation new methodsWithUnboundGlobals.
	
	self 
		assert: methodsWithUnboundGlobals isEmpty
		description: [ String streamContents: [ :s |
			s 
				nextPutAll: 'Found methods with unbound globals: '; 
				print: methodsWithUnboundGlobals ]]