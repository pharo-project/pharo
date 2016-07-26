serialize/materialize
serializeInstance: aCompiledMethod with: anEncoder
	| methodClass |
	methodClass := aCompiledMethod methodClass.
	self serializeGlobalClass: methodClass instanceSide on: anEncoder.
	anEncoder encodeByte: (methodClass isClassSide ifTrue: [1] ifFalse: [0]).
	anEncoder encodeString: aCompiledMethod selector.
	anEncoder encodeUint16: aCompiledMethod bytecodesHash.