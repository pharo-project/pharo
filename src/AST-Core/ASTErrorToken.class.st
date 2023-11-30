"
I'm a scanned error.
I can have multiple causes.
"
Class {
	#name : 'ASTErrorToken',
	#superclass : 'ASTValueToken',
	#instVars : [
		'cause',
		'location'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASTErrorToken class >> initialize [

	self deprecatedAliases: { #ASTErrorToken }
]

{ #category : 'instance creation' }
ASTErrorToken class >> value: value start: tokenStart cause: errorCause location: thePosition [
	^ self new
		value: value
			start: tokenStart
			cause: errorCause
			location: thePosition;
		yourself
]

{ #category : 'accessing' }
ASTErrorToken >> cause [
	^ cause
]

{ #category : 'testing' }
ASTErrorToken >> isError [
	^true
]

{ #category : 'accessing' }
ASTErrorToken >> location [
	^ location
]

{ #category : 'accessing' }
ASTErrorToken >> location: anInteger [
	location := anInteger
]

{ #category : 'accessing' }
ASTErrorToken >> value: theValue start: tokenStart cause: errorCause location: errorPosition [
	self value: theValue start: tokenStart.
	location := errorPosition.
	cause := errorCause
]
