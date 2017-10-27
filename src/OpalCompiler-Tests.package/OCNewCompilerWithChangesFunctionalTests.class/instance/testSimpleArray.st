test - array compilation
testSimpleArray
	
	| aCompiledMethod bytecode index |
	aCompiledMethod := OpalCompiler new
									encoderClass:  EncoderForV3PlusClosures; 
									source: 'method
	^{1}';
									class: OCMockCompilationClass;
									compile.
									
	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	index := 0.
	self assert: (bytecode at: (index := index + 1)) = '13 <76> pushConstant: 1'.
	self assert: (bytecode at: (index := index + 1)) = '14 <8A 81> pop 1 into (Array new: 1)'.
	self assert: (bytecode at: (index := index + 1)) = '16 <7C> returnTop'.
