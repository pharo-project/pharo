Class {
	#name : 'OCEOFToken',
	#superclass : 'OCToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCEOFToken >> isEOF [
	^true
]

{ #category : 'accessing' }
OCEOFToken >> length [
	^ 0
]

{ #category : 'accessing' }
OCEOFToken >> source [
	^ self value
]

{ #category : 'accessing' }
OCEOFToken >> value [

	^ ''
]
