testing-primitives
testPrimitiveNamed2
	"This test useses the #primPathNameDelimiter primitive."

	self compile: '<primitive: ''primitiveDirectoryDelimitor'' module: ''FilePlugin''> ^ #delim' selector: #delim.
	self assert: self delim equals: FilePluginPrims new delimiter.
	
