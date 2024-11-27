"
ASTAssignmentToken is the first-class representation of the assignment token ':='

"
Class {
	#name : 'ASTAssignmentToken',
	#superclass : 'ASTToken',
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'testing' }
ASTAssignmentToken >> isAssignment [
	^true
]

{ #category : 'accessing' }
ASTAssignmentToken >> length [

	^ 2
]

{ #category : 'evaluating' }
ASTAssignmentToken >> value [
	^':='
]
