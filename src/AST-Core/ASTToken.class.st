"
ASTToken is the abstract superclass of all of the RB tokens. These tokens (unlike the standard parser's) remember where they came from in the original source code.

Subclasses must implement the following messages:
	accessing
		length

Instance Variables:
	sourcePointer	<Integer>	The position in the original source code where this token began.

"
Class {
	#name : 'ASTToken',
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
ASTToken class >> isAbstract [

	^ self == ASTToken
]

{ #category : 'instance creation' }
ASTToken class >> start: anInterval [
	^self new start: anInterval
]

{ #category : 'accessing' }
ASTToken >> comments [
	^comments
]

{ #category : 'accessing' }
ASTToken >> comments: anObject [
	comments := anObject
]

{ #category : 'testing' }
ASTToken >> isAssignment [
	^false
]

{ #category : 'testing' }
ASTToken >> isBinary [
	^false
]

{ #category : 'testing' }
ASTToken >> isBinary: aString [
	^false
]

{ #category : 'testing' }
ASTToken >> isComment [
	^false
]

{ #category : 'testing' }
ASTToken >> isEOF [
	^false
]

{ #category : 'testing' }
ASTToken >> isError [
	^false
]

{ #category : 'testing' }
ASTToken >> isIdentifier [
	^false
]

{ #category : 'testing' }
ASTToken >> isKeyword [
	^false
]

{ #category : 'testing' }
ASTToken >> isKeywordPattern [
	^ false
]

{ #category : 'testing' }
ASTToken >> isLiteralArrayToken [
	^false
]

{ #category : 'testing' }
ASTToken >> isLiteralToken [
	^false
]

{ #category : 'testing' }
ASTToken >> isNumberLiteralToken [
	^false
]

{ #category : 'testing' }
ASTToken >> isPatternBlock [
	^false
]

{ #category : 'testing' }
ASTToken >> isPatternVariable [
	^false
]

{ #category : 'testing' }
ASTToken >> isPrimitiveKeyword [
	^ false
]

{ #category : 'testing' }
ASTToken >> isSpecial [
	^false
]

{ #category : 'testing' }
ASTToken >> isSpecial: aValue [
	^false
]

{ #category : 'testing' }
ASTToken >> isTouching: other [
	"is self just before other token (without space or comments)"
	
	^ self stop + 1 = other start
]

{ #category : 'accessing' }
ASTToken >> length [
	^self subclassResponsibility
]

{ #category : 'printing' }
ASTToken >> printOn: aStream [
	aStream
		nextPut: $ ;
		nextPutAll: self class name
]

{ #category : 'accessing' }
ASTToken >> removePositions [
	sourcePointer := nil
]

{ #category : 'accessing' }
ASTToken >> start [
	^ sourcePointer ifNil: [ 0 ]
]

{ #category : 'initialization' }
ASTToken >> start: anInteger [
	sourcePointer := anInteger
]

{ #category : 'accessing' }
ASTToken >> stop [
	^ sourcePointer
		ifNil: [ -1 ]
		ifNotNil: [ self start + self length - 1 ]
]
