printing
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primDecl |
	self isPrimitive ifFalse: [ ^self ].

	self isExternalCallPrimitive ifTrue:
		[^aStream print: (self literalAt: 1); cr].
	aStream nextPutAll: '<primitive: '.
	self isNamedPrimitive
		ifTrue:
			[primDecl := self literalAt: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: self primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; cr