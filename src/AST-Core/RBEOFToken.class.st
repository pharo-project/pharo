Class {
	#name : #RBEOFToken,
	#superclass : #RBToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBEOFToken >> isEOF [
	^true
]

{ #category : #accessing }
RBEOFToken >> length [
	^ 0
]

{ #category : #accessing }
RBEOFToken >> value [

	^ ''
]
