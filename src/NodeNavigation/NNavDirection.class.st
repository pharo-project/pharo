"
I'm the one who knows generalization to navigate an ast tree
I have two concret classes: NavigateInto and NavigateOver
"
Class {
	#name : #NNavDirection,
	#superclass : #Object,
	#classVars : [
		'Instance'
	],
	#category : 'NodeNavigation-Core'
}

{ #category : #private }
NNavDirection class >> calculateBestSeletionFor: node beforeSelection: selectionInText step: blockForStep [
	| selectionInNode |
	
	node ifNil:[ ^selectionInText ].
	selectionInNode := node start to: node stop.
	^ (selectionInText = selectionInNode )
		ifTrue: [blockForStep value: selectionInText  value: node ]
		ifFalse: [selectionInNode ].
]

{ #category : #'stand out' }
NNavDirection class >> findSelectionFor: navigationContext [
	self subclassResponsibility .
]
