"
RBAssignmentToken is the first-class representation of the assignment token ':='

"
Class {
	#name : #RBAssignmentToken,
	#superclass : #RBToken,
	#category : #'AST-Core-Tokens'
}

{ #category : #testing }
RBAssignmentToken >> isAssignment [
	^true
]

{ #category : #accessing }
RBAssignmentToken >> length [

	^ 2
]

{ #category : #evaluating }
RBAssignmentToken >> value [
	^':='
]
