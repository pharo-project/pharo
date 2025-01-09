"
I represent a syntax or a semantic error in some pharo code.

My instances are signaled by the parser or the compiler.
"
Class {
	#name : 'OCCodeError',
	#superclass : 'Error',
	#instVars : [
		'notice'
	],
	#category : 'AST-Core-Exception',
	#package : 'AST-Core',
	#tag : 'Exception'
}

{ #category : 'accessing' }
OCCodeError >> description [

	^ self class name , ':' , self notice description
]

{ #category : 'private - testing' }
OCCodeError >> isResumable [

	"When resumed, faulty AST are produced"
	^true
]

{ #category : 'accessing' }
OCCodeError >> location [

	^ self position
]

{ #category : 'accessing' }
OCCodeError >> messageText [

	^ self notice messageText
]

{ #category : 'accessing' }
OCCodeError >> methodClass [
	^ self methodNode methodClass
]

{ #category : 'accessing' }
OCCodeError >> methodNode [
	^ self node methodNode
]

{ #category : 'accessing' }
OCCodeError >> node [
	^ self notice node
]

{ #category : 'accessing' }
OCCodeError >> notice [

	^ notice
]

{ #category : 'accessing' }
OCCodeError >> notice: anObject [

	notice := anObject
]

{ #category : 'accessing' }
OCCodeError >> position [

	^ self notice position
]

{ #category : 'accessing' }
OCCodeError >> sourceCode [

	^ self methodNode sourceCode
]
