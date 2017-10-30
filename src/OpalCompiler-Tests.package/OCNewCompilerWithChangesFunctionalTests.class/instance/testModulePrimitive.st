other-tests
testModulePrimitive
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new 
	encoderClass:  EncoderForV3PlusClosures; 
	source: 'copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>
	self primitiveFailed';
	class: MockForCompilation;
	compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '<primitive: ''primitiveCopyBits'' module: ''BitBltPlugin''>'.
	self assert: (bytecode at: (index := index + 1)) = '21 <8B 75 00> callPrimitive: 117'.
	self assert: (bytecode at: (index := index + 1)) = '24 <70> self'.
	self assert: (bytecode at: (index := index + 1)) = '25 <D1> send: primitiveFailed'.
	self assert: (bytecode at: (index := index + 1)) = '26 <87> pop'.
	self assert: (bytecode at: (index := index + 1)) = '27 <78> returnSelf'.	

	