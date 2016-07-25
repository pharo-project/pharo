printing
printPrimitiveOn: aStream
	"Print the primitive on aStream"

	| primIndex primDecl |
	primIndex := primitiveNum.
	primIndex = 0
		ifTrue: [ ^ self ].
	primIndex = 120
		ifTrue: [ 
			"External call spec"
			^ aStream print: spec ].
	aStream nextPutAll: '<primitive: '.
	primIndex = 117
		ifTrue: [ 
			primDecl := spec.
			aStream
				nextPut: $';
				nextPutAll: primDecl second;
				nextPut: $'.
			primDecl first
				ifNotNil: [ 
					aStream
						nextPutAll: ' module: ';
						nextPut: $';
						nextPutAll: primDecl first;
						nextPut: $' ] ]
		ifFalse: [ aStream print: primIndex ].
	aStream nextPut: $>.
	(primIndex ~= 117 and: [ primIndex ~= 120 ])
		ifTrue: [ 
			Smalltalk
				at: #Interpreter
				ifPresent: [ :cls | 
					aStream
						nextPutAll: ' "';
						nextPutAll: ((cls classPool at: #PrimitiveTable) at: primIndex + 1);
						nextPutAll: '" ' ] ]