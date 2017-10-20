visiting
visitMethod: aMethod
	| aHEMethod compiledMethod |
	aHEMethod  := HEMethod new.
	aHEMethod name: aMethod selector.
	aHEMethod className: aMethod methodClass name.
	aHEMethod protocol: aMethod protocol.

	compiledMethod := self compileMethod: aMethod.

	aHEMethod fillLiteralsAndBytecode: compiledMethod.
	
	^ aHEMethod