"
A FinderExampleClassNode is a node used by the FinderUI's tree representing the Class for the method deduced
from an examples (by MethodFinder)
"
Class {
	#name : #FinderExampleClassNode,
	#superclass : #FinderClassNode,
	#category : #'Tool-Finder'
}

{ #category : #displaying }
FinderExampleClassNode >> displayString [
	(self parentNode receiver isKindOf: self item)
		ifTrue: [ ^ '*' , super displayString ].
	^ super displayString
]
