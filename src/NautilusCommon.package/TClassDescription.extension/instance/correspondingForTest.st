*NautilusCommon
correspondingForTest
	"Return the unit test that correspond to me. If it does not exist, it returns myself. 
	 Return the tested class if sent to a class"
	
	| className |
	className := (self inheritsFrom: TestCase)
		ifTrue: [ self name copyReplaceAll: 'Test' with: '' ]
		ifFalse: [ self name, 'Test' ].
	^ Smalltalk 
		at: className asSymbol 
		ifAbsent: [ self ]
	