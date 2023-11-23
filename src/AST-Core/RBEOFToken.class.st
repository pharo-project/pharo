Class {
	#name : 'RBEOFToken',
	#superclass : 'RBToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
RBEOFToken >> isEOF [
	^true
]

{ #category : 'accessing' }
RBEOFToken >> length [
	^ 0
]

{ #category : 'accessing' }
RBEOFToken >> source [
	^ self value
]

{ #category : 'accessing' }
RBEOFToken >> value [

	^ ''
]
