Class {
	#name : #CoMockASTResultSetBuilder,
	#superclass : #CoASTResultSetBuilder,
	#instVars : [
		'heuristic'
	],
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #accessing }
CoMockASTResultSetBuilder >> heuristic [

	^ heuristic
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitArrayNode: aRBArrayNode [

	heuristic := #array.
	^ super visitArrayNode: aRBArrayNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitAssignmentNode: aRBAssignmentNode [

	heuristic := #assignment.
	^ super visitAssignmentNode: aRBAssignmentNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitBlockNode: aNode [

	heuristic := #block.
	^ super visitBlockNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitCascadeNode: aRBCascadeNode [ 

	heuristic := #cascade.
	^ super visitCascadeNode: aRBCascadeNode 
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitLiteralArrayNode: aNode [

	heuristic := #literalArray.
	^ super visitLiteralArrayNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitLiteralValueNode: aNode [

	heuristic := #literal.
	^ super visitLiteralValueNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitMessageNode: aNode [

	heuristic := #message.
	^ super visitMessageNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitMethodNode: aNode [

	heuristic := #method.
	^ super visitMethodNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitParseErrorNode: aNode [

	heuristic := #error.
	^ super visitParseErrorNode: aNode
]

{ #category : #visiting }
CoMockASTResultSetBuilder >> visitPragmaNode: aRBPragmaNode [ 

	heuristic := #pragma.
	^ super visitPragmaNode: aRBPragmaNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitReturnNode: aNode [

	heuristic := #return.
	^ super visitReturnNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitSequenceNode: aNode [

	heuristic := #sequence.
	^ super visitSequenceNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitThisContextNode: aNode [

	heuristic := #thisContext.
	^ super visitThisContextNode: aNode
]

{ #category : #accessing }
CoMockASTResultSetBuilder >> visitVariableNode: aNode [

	heuristic := #variable.
	^ super visitVariableNode: aNode
]
