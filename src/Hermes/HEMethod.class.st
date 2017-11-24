Class {
	#name : #HEMethod,
	#superclass : #HEAbstractExported,
	#instVars : [
		'className',
		'name',
		'literals',
		'bytecode',
		'protocol',
		'encoderClassName',
		'numArgs',
		'numTemps',
		'hasPrimitive'
	],
	#category : #'Hermes-Model'
}

{ #category : #'instance creation' }
HEMethod class >> for: aMethod [
	^ self new
		fromMethod: aMethod;
		yourself
]

{ #category : #accessing }
HEMethod >> bytecode [
	^ bytecode
]

{ #category : #accessing }
HEMethod >> bytecode: anObject [
	bytecode := anObject
]

{ #category : #accessing }
HEMethod >> className [
	^ className
]

{ #category : #accessing }
HEMethod >> className: anObject [
	className := anObject
]

{ #category : #'instance creation' }
HEMethod >> fillLiteralsAndBytecode: aMethod [
	| literalSpace |
	
	encoderClassName := aMethod encoderClass name.
	numArgs := aMethod numArgs.
	numTemps := aMethod numTemps.
	hasPrimitive := aMethod isPrimitive asBit.

	literalSpace := (aMethod numLiterals + 1) * Smalltalk wordSize.

	bytecode := ByteArray new: aMethod size - literalSpace.
	1 to: bytecode size do: [ :i | bytecode at: i put: (aMethod at: i + literalSpace) ].

	literals := aMethod literals allButLast collect: [ :e | e asExportedLiteral ].
	"The last literal is the binding of the method with the holding class, it is easy to reconstruct"
	literals := literals copyWith: nil asExportedLiteral.
]

{ #category : #'instance creation' }
HEMethod >> fromMethod: aMethod [
	name := aMethod selector.
	className := aMethod methodClass name.
	protocol := aMethod protocol.
	self fillLiteralsAndBytecode: aMethod
]

{ #category : #compiling }
HEMethod >> headerFor: anEnvironment [
	| header encoderClass|
	
	encoderClass := anEnvironment classNamed: encoderClassName.
	
	^ (CompiledMethod headerFlagForEncoder: encoderClass) + (numArgs bitShift: 24) + (numTemps bitShift: 18) + literals size + (hasPrimitive bitShift: 16)
]

{ #category : #accessing }
HEMethod >> literals [
	^ literals
]

{ #category : #accessing }
HEMethod >> literals: anObject [
	literals := anObject
]

{ #category : #accessing }
HEMethod >> name [
	^ name
]

{ #category : #accessing }
HEMethod >> name: anObject [
	name := anObject
]

{ #category : #printing }
HEMethod >> printOn: aStream [
	aStream
		nextPutAll: 'HEMethod ( ';
		nextPutAll: name printString;
		nextPutAll: ' )'
]

{ #category : #accessing }
HEMethod >> protocol [
	^ protocol
]

{ #category : #accessing }
HEMethod >> protocol: anObject [
	protocol := anObject
]

{ #category : #reading }
HEMethod >> readFrom: aReader [
	| numberOfLiterals |

	name := aReader readByteSymbol.
	className := aReader readByteSymbol.
	protocol := aReader readByteSymbol.
		
	encoderClassName := aReader readByteString.
	(encoderClassName beginsWith: 'Opal') ifTrue: [ 
		encoderClassName := encoderClassName withoutPrefix: 'Opal' ].
	numArgs := aReader readUInt32.
	numTemps := aReader readUInt32.
	hasPrimitive := aReader readUInt32.

	bytecode := aReader readByteArray.

	numberOfLiterals := aReader readInt32.
	literals := Array new: numberOfLiterals.
	1 to: numberOfLiterals do: [ :idx | literals at:idx put: (HEExportedLiteral readFrom: aReader) ]
]

{ #category : #writing }
HEMethod >> writeInto: aWriter [
	aWriter writeByteString: name.
	aWriter writeByteString: className.
	aWriter writeByteString: protocol.

	aWriter writeByteString: encoderClassName.
	aWriter writeUInt32: numArgs.
	aWriter writeUInt32: numTemps.
	aWriter writeUInt32: hasPrimitive.

	aWriter writeByteArray: bytecode.
	
	aWriter writeInt32: literals size.
	literals do:[:e | e writeInto: aWriter].
]
