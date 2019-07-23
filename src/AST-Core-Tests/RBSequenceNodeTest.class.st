Class {
	#name : #RBSequenceNodeTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Nodes'
}

{ #category : #tests }
RBSequenceNodeTest >> testStop [
| ast |
ast := RBParser parseMethod: 'method | temp |'.
self assert: ast body stop equals: ast body temporaries last stop.
]
