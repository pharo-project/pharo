"
ASTAssignmentToken is the first-class representation of the assignment token ':='

"
Class {
	#name : 'OCAssignmentToken',
	#superclass : 'OCToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
OCAssignmentToken >> isAssignment [
	^true
]

{ #category : 'accessing' }
OCAssignmentToken >> length [

	^ 2
]

{ #category : 'evaluating' }
OCAssignmentToken >> value [
	^':='
]
