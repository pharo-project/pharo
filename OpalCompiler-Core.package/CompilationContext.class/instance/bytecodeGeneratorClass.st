accessing
bytecodeGeneratorClass
	^ bytecodeGeneratorClass ifNil: [ bytecodeGeneratorClass := IRBytecodeGenerator ]