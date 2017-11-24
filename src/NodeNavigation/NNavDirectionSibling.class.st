"
I'm the one who knows how to navigate in an horizontal way visiting node in the same level statin from left to right 
"
Class {
	#name : #NNavDirectionSibling,
	#superclass : #NNavDirection,
	#category : #'NodeNavigation-Core'
}

{ #category : #'step over' }
NNavDirectionSibling class >> brotherNodeFor: node [
"Finds the next brother for the node"
	| brothers nodePosition nextBrotherPosition |

	node ifNil: [ ^nil ].
	node parent ifNil: [ ^nil ]. "no parent you are orphan"
	brothers := node parent children.
	nodePosition := (brothers indexOf: node) .
	nextBrotherPosition := self brotherPositionFor: nodePosition brothers: brothers .
	^ brothers at: nextBrotherPosition.

]

{ #category : #'step over' }
NNavDirectionSibling class >> brotherPositionFor: nodePosition brothers: brothers [
	^ nodePosition = brothers size ifTrue:  [ 1 ] ifFalse: [ nodePosition + 1 ].

]

{ #category : #builder }
NNavDirectionSibling class >> buildShortcut: aBuilder [
	<keymap>

	(aBuilder shortcut: #sibling)
		category: #SmalltalkEditor
		default: self defaultKeyCombination 
		do: [ :morph | morph standOutHorizontalScope]
		description: 'Select the node going to the siblings'



]

{ #category : #builder }
NNavDirectionSibling class >> defaultKeyCombination [
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [ Character arrowRight meta shift  ]
		ifFalse: [ $u meta shift  ]
]

{ #category : #'stand out' }
NNavDirectionSibling class >> findSelectionFor: navigationContext [
	^ self
		calculateBestSeletionFor: (self brotherNodeFor: navigationContext bestNodeForSelection )
		beforeSelection: navigationContext textSelection 
		step: [ :selection :target | self selectionBrotherScope: selection for: target ]
]

{ #category : #'step over' }
NNavDirectionSibling class >> selectionBrotherScope: selected for: node [
	| brother |

	node ifNil: [ ^selected ].
	brother := self brotherNodeFor:  node.
	^brother ifNil:[nil] ifNotNil: [ brother start to: brother stop].
]
