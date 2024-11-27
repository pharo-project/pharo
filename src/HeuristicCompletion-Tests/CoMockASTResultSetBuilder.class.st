"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'CoMockASTResultSetBuilder',
	#superclass : 'CoASTResultSetBuilder',
	#instVars : [
		'heuristic'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> heuristic [

	^ heuristic
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitArrayNode: anArrayNode [

	heuristic := #array.
	^ super visitArrayNode: anArrayNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitAssignmentNode: aAssignmentNode [

	heuristic := #assignment.
	^ super visitAssignmentNode: aAssignmentNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitBlockNode: aNode [

	heuristic := #block.
	^ super visitBlockNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitCascadeNode: aCascadeNode [

	heuristic := #cascade.
	^ super visitCascadeNode: aCascadeNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitLiteralArrayNode: aNode [

	heuristic := #literalArray.
	^ super visitLiteralArrayNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitLiteralValueNode: aNode [

	heuristic := #literal.
	^ super visitLiteralValueNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitMessageNode: aNode [

	heuristic := #message.
	^ super visitMessageNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitMethodNode: aNode [

	heuristic := #method.
	^ super visitMethodNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitParseErrorNode: aNode [

	heuristic := #error.
	^ super visitParseErrorNode: aNode
]

{ #category : 'visiting' }
CoMockASTResultSetBuilder >> visitPragmaNode: aPragmaNode [

	heuristic := #pragma.
	^ super visitPragmaNode: aPragmaNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitReturnNode: aNode [

	heuristic := #return.
	^ super visitReturnNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitSequenceNode: aNode [

	heuristic := #sequence.
	^ super visitSequenceNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitThisContextNode: aNode [

	heuristic := #thisContext.
	^ super visitThisContextNode: aNode
]

{ #category : 'accessing' }
CoMockASTResultSetBuilder >> visitVariableNode: aNode [

	heuristic := #variable.
	^ super visitVariableNode: aNode
]
