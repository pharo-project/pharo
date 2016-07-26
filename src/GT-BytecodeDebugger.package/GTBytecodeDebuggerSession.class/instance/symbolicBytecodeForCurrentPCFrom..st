acessing
symbolicBytecodeForCurrentPCFrom: aContext
	| pc |
	
	pc := aContext pc.
	^ aContext method symbolicBytecodes 
		detect: [ :each | each offset = pc ]
		ifNone: [ nil ]