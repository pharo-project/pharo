"
ASTToken is the abstract superclass of all of the RB tokens. These tokens (unlike the standard parser's) remember where they came from in the original source code.

Subclasses must implement the following messages:
	accessing
		length

Instance Variables:
	sourcePointer	<Integer>	The position in the original source code where this token began.

"
Class {
	#name : 'OCToken',
	#superclass : 'Object',
	#instVars : [
		'sourcePointer',
		'comments'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCToken class >> isAbstract [

	^ self == OCToken
]

{ #category : 'instance creation' }
OCToken class >> start: anInterval [
	^self new start: anInterval
]

{ #category : 'accessing' }
OCToken >> comments [
	^comments
]

{ #category : 'accessing' }
OCToken >> comments: anObject [
	comments := anObject
]

{ #category : 'testing' }
OCToken >> isAssignment [
	^false
]

{ #category : 'testing' }
OCToken >> isBinary [
	^false
]

{ #category : 'testing' }
OCToken >> isBinary: aString [
	^false
]

{ #category : 'testing' }
OCToken >> isComment [
	^false
]

{ #category : 'testing' }
OCToken >> isEOF [
	^false
]

{ #category : 'testing' }
OCToken >> isError [
	^false
]

{ #category : 'testing' }
OCToken >> isIdentifier [
	^false
]

{ #category : 'testing' }
OCToken >> isKeyword [
	^false
]

{ #category : 'testing' }
OCToken >> isKeywordPattern [
	^ false
]

{ #category : 'testing' }
OCToken >> isLiteralArrayToken [
	^false
]

{ #category : 'testing' }
OCToken >> isLiteralToken [
	^false
]

{ #category : 'testing' }
OCToken >> isNumberLiteralToken [
	^false
]

{ #category : 'testing' }
OCToken >> isPatternBlock [
	^false
]

{ #category : 'testing' }
OCToken >> isPatternVariable [
	^false
]

{ #category : 'testing' }
OCToken >> isPrimitiveKeyword [
	^ false
]

{ #category : 'testing' }
OCToken >> isSpecial [
	^false
]

{ #category : 'testing' }
OCToken >> isSpecial: aValue [
	^false
]

{ #category : 'testing' }
OCToken >> isTouching: other [
	"is self just before other token (without space or comments)"
	
	^ self stop + 1 = other start
]

{ #category : 'accessing' }
OCToken >> length [
	^self subclassResponsibility
]

{ #category : 'printing' }
OCToken >> printOn: aStream [
	aStream
		nextPut: $ ;
		nextPutAll: self class name
]

{ #category : 'accessing' }
OCToken >> removePositions [
	sourcePointer := nil
]

{ #category : 'accessing' }
OCToken >> start [
	^ sourcePointer ifNil: [ 0 ]
]

{ #category : 'initialization' }
OCToken >> start: anInteger [
	sourcePointer := anInteger
]

{ #category : 'accessing' }
OCToken >> stop [
	^ sourcePointer
		ifNil: [ -1 ]
		ifNotNil: [ self start + self length - 1 ]
]
