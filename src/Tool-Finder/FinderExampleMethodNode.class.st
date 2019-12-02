"
A FinderExampleMethodNode is a node used by the FinderUI's tree representing a Method deduced
from an examples (by MethodFinder)
"
Class {
	#name : #FinderExampleMethodNode,
	#superclass : #FinderMethodNode,
	#category : #'Tool-Finder-Tree'
}

{ #category : #accessing }
FinderExampleMethodNode >> childNodeClassFromItem: anItem [
	"I answer the class of my children nodes (if I have some)"

	^ FinderExampleClassNode
]

{ #category : #private }
FinderExampleMethodNode >> receiver [
	"return the receiver of my item."
	^(RBParser parseExpression: self item) receiver evaluate
]

{ #category : #private }
FinderExampleMethodNode >> selector [
	^ self model finder findSelector: self item
]
