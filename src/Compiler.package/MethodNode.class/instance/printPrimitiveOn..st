printing
printPrimitiveOn: aStream
	"Print the primitive on aStream"

	| primDecl |
	primitive = 0
		ifTrue: [ ^ self ].
	primitive = 120
		ifTrue: [ 
			"External call spec"
			^ aStream print: encoder literals first ].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue: [ 
			primDecl := encoder literals at: 1.
			(primDecl at: 2) asString printOn: aStream.
			(primDecl at: 1)
				ifNotNil: [ :moduleName | 
					aStream nextPutAll: ' module: '.
					moduleName asString printOn: aStream ] ]
		ifFalse: [ aStream print: primitive ].
	self primitiveErrorVariableName
		ifNotNil: [ :primitiveErrorVariableName | 
			aStream
				nextPutAll: ' error: ';
				nextPutAll: primitiveErrorVariableName ].
	aStream nextPut: $>.