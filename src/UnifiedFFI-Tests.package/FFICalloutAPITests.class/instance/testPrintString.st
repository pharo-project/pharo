tests atomic
testPrintString
	"Test using generic FFI spec"
	| result buffer |
	
	buffer := ByteArray new: 12.
	result := self ffiCopyString: 'Hello World!' to: buffer.
	self assert: result equals: 'Hello World!'.
	self assert: buffer asString equals: 'Hello World!'.	