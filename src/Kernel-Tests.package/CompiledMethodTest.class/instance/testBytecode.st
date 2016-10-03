tests - accessing
testBytecode
	"The result of this test depends on the used bytecode set. 
	
	 Because there are multiple versions of the encoders currently depending on the compiler used, we test the class name instead of the encoder class itself"
	| method expectedResult |
	method := Object>>#halt.
	expectedResult := (method encoderClass name endsWith: 'SistaV1')
		ifTrue: [#[16 129 216 88]]
		ifFalse: [#[64 209 135 120]].
	self assertCollection: (Object>>#halt) bytecode equals: expectedResult