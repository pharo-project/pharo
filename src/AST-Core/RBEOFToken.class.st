Class {
	#name : #RBEOFToken,
	#superclass : #RBToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBEOFToken >> isEOF [
	^true
]

{ #category : #testing }
RBEOFToken >> length [
	^ 0
]

{ #category : #testing }
RBEOFToken >> value [

	^ ''
]
