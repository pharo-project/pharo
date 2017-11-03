"
I'm the one who knows how to navigate going to the parent node.
"
Class {
	#name : #NNavDirectionParent,
	#superclass : #NNavDirection,
	#category : #NodeNavigation-Core
}

{ #category : #builder }
NNavDirectionParent class >> buildShortcut: aBuilder [
	<keymap>

	(aBuilder shortcut: #parent)
		category: #SmalltalkEditor
		default: self defaultKeyCombination
		do: [ :morph | morph standOutOverScope ]
		description: 'Select the node scope going to the paren node'



]

{ #category : #builder }
NNavDirectionParent class >> defaultKeyCombination [
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [ Character arrowLeft meta shift ]
		ifFalse: [ $p meta shift  ]


]

{ #category : #'stand out' }
NNavDirectionParent class >> findSelectionFor: navigationContext [
	^ self
		calculateBestSeletionFor: navigationContext bestNodeForSelection 
		beforeSelection: navigationContext textSelection 
		step: [ :selection :target | self selectionParentScope: selection for: target parent ]
]

{ #category : #'step over' }
NNavDirectionParent class >> selectionParentScope: selected for: node [
	| nodeSelection |

	node ifNil: [ ^selected ].
	nodeSelection := node start to: node stop.
	selected = nodeSelection
		ifTrue: [ ^self selectionParentScope: selected for: node parent  ]
		ifFalse: [ ^nodeSelection ].
]
