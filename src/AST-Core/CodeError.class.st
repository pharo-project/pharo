"
I represent a syntax or a semantic error in some pharo code.

My instances are signaled by the parser or the compiler.
"
Class {
	#name : 'CodeError',
	#superclass : 'Error',
	#instVars : [
		'notice'
	],
	#category : 'AST-Core-Exception',
	#package : 'AST-Core',
	#tag : 'Exception'
}

{ #category : 'accessing' }
CodeError >> description [

	^ self class name , ':' , self notice description
]

{ #category : 'accessing' }
CodeError >> errorCode [

	self
		deprecated: 'errorCode is so misleading'
		transformWith: '`@receiver errorCode' -> '`@receiver sourceCode'.

	^ self sourceCode
]

{ #category : 'accessing' }
CodeError >> errorMessage [

	self
		deprecated: 'As with exceptions, prefer messageText'
		transformWith: '`@receiver errorMessage' -> '`@receiver messageText'.

	^ messageText
]

{ #category : 'private - testing' }
CodeError >> isResumable [

	"When resumed, faulty AST are produced"
	^true
]

{ #category : 'accessing' }
CodeError >> location [

	^ self position
]

{ #category : 'accessing' }
CodeError >> messageText [

	^ self notice messageText
]

{ #category : 'accessing' }
CodeError >> methodClass [
	^ self methodNode methodClass
]

{ #category : 'accessing' }
CodeError >> methodNode [
	^ self node methodNode
]

{ #category : 'accessing' }
CodeError >> node [
	^ self notice node
]

{ #category : 'accessing' }
CodeError >> notice [

	^ notice
]

{ #category : 'accessing' }
CodeError >> notice: anObject [

	notice := anObject
]

{ #category : 'accessing' }
CodeError >> position [

	^ self notice position
]

{ #category : 'accessing' }
CodeError >> sourceCode [

	^ self methodNode sourceCode
]
