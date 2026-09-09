"
ASTValueToken is the abstract superclass of all tokens that have additional information attached. For example, the BinarySelector token holds onto the actual character (e.g. $+).

Instance Variables:
	value	<String>	The value of this token


"
Class {
	#name : 'OCValueToken',
	#superclass : 'OCToken',
	#instVars : [
		'value'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCValueToken class >> isAbstract [

	^ self == OCValueToken
]

{ #category : 'instance creation' }
OCValueToken class >> value: aString start: anInteger [
	^self new value: aString start: anInteger
]

{ #category : 'private' }
OCValueToken >> length [
	^value size
]

{ #category : 'printing' }
OCValueToken >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	value printOn: aStream.
	aStream nextPutAll: ')'
]

{ #category : 'accessing' }
OCValueToken >> source [
	^ self value
]

{ #category : 'accessing' }
OCValueToken >> value [
	^value
]

{ #category : 'accessing' }
OCValueToken >> value: anObject [
	value := anObject
]

{ #category : 'initialization' }
OCValueToken >> value: aString start: anInteger [
	value := aString.
	sourcePointer := anInteger
]
