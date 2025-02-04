"
I represent a primitive.  I am more than just a number if I am a named primitive.

## Instance variables

- num	<Integer>	 Primitive number.
-  spec <Object> Stored in first literal when num is 117 or 120.

"
Class {
	#name : 'OCIRPrimitive',
	#superclass : 'Object',
	#instVars : [
		'primitiveNum',
		'spec'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'initialization' }
OCIRPrimitive class >> from: aPragmaNode [

	^ self new initializeFrom: aPragmaNode
]

{ #category : 'instance creation' }
OCIRPrimitive class >> null [

	^ self new num: 0
]

{ #category : 'accessing' }
OCIRPrimitive >> initializeFrom: aPragmaNode [

	primitiveNum := 0.
	spec := Array
		with: (aPragmaNode arguments second) name
		with: (aPragmaNode arguments first) name
		with: 0 with: 0
]

{ #category : 'accessing' }
OCIRPrimitive >> num [

	^ primitiveNum
]

{ #category : 'accessing' }
OCIRPrimitive >> num: n [

	primitiveNum := n
]

{ #category : 'printing' }
OCIRPrimitive >> printOn: aStream [

	aStream nextPutAll: 'primitive '; print: primitiveNum
]

{ #category : 'printing' }
OCIRPrimitive >> printPrimitiveOn: aStream [
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
	aStream nextPut: $>
]

{ #category : 'printing' }
OCIRPrimitive >> sourceText [

	^ String streamContents: [:stream |
		self printPrimitiveOn: stream]
]

{ #category : 'accessing' }
OCIRPrimitive >> spec [

	^ spec
]

{ #category : 'accessing' }
OCIRPrimitive >> spec: literal [

	spec := literal
]
