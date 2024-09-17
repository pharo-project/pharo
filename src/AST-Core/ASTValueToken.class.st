"
RBValueToken is the abstract superclass of all tokens that have additional information attached. For example, the BinarySelector token holds onto the actual character (e.g. $+).

Instance Variables:
	value	<String>	The value of this token


"
Class {
	#name : 'ASTValueToken',
	#superclass : 'ASTToken',
	#instVars : [
		'value'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
ASTValueToken class >> isAbstract [

	^ self == ASTValueToken
]

{ #category : 'instance creation' }
ASTValueToken class >> value: aString start: anInteger [
	^self new value: aString start: anInteger
]

{ #category : 'private' }
ASTValueToken >> length [
	^value size
]

{ #category : 'printing' }
ASTValueToken >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	value printOn: aStream.
	aStream nextPutAll: ')'
]

{ #category : 'accessing' }
ASTValueToken >> source [
	^ self value
]

{ #category : 'accessing' }
ASTValueToken >> value [
	^value
]

{ #category : 'accessing' }
ASTValueToken >> value: anObject [
	value := anObject
]

{ #category : 'initialization' }
ASTValueToken >> value: aString start: anInteger [
	value := aString.
	sourcePointer := anInteger
]
