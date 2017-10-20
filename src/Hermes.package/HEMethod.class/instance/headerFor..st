compiling
headerFor: anEnvironment
	| header encoderClass|
	
	encoderClass := anEnvironment classNamed: encoderClassName.
	
	^ (CompiledMethod headerFlagForEncoder: encoderClass) + (numArgs bitShift: 24) + (numTemps bitShift: 18) + literals size + (hasPrimitive bitShift: 16)