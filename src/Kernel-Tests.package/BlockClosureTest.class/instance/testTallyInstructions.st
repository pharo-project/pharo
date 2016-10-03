tests
testTallyInstructions
	"Depends on compiler settings... If everything is compiled to the V3PlusClosures bytecode set, then the result is 27.
	 If everything is compiled to the SistaV1 bytecode set, then the result is 26. If execution is mixed between method
	compiled with V3PlusClosures and SistaV1, result is then different and untested."
	
	"We test class name and not the class because there are multiple versions of the encoders depending on the compiler used."
	| expectedResult |
	expectedResult := (aBlockContext method encoderClass name endsWith: 'SistaV1')
		ifTrue: [ 26 ]
		ifFalse: [ 27 ].
	self assert: (Context tallyInstructions: aBlockContext ) size equals: expectedResult