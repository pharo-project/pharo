"
I'm the one who knowshow to navigate inside the child nodes
"
Class {
	#name : #NNavDirectionChild,
	#superclass : #NNavDirection,
	#category : 'NodeNavigation-Core'
}

{ #category : #private }
NNavDirectionChild class >> askForNodeSelectionFrom: nodes [
	
	
	^ UIManager default 
		enterOrRequestFrom: (nodes collect: [:node | (node respondsTo: #selector )ifTrue:[node selector] ifFalse:[node name]])
		values: nodes 
		lines: #() 
		title: 'Which code do you want to select?' translated
]

{ #category : #builder }
NNavDirectionChild class >> buildShortcut: aBuilder [
	<keymap>
	(aBuilder shortcut: #selectChild)
		category: #SmalltalkEditor
		default: $o meta
		do: [ :morph | morph standOutIntoScope ]
		description: 'Select the node choosing the child'
]

{ #category : #private }
NNavDirectionChild class >> childNodeFor: node [
	"We find the child for a node, if it has not childs nil, if has more than one let the user select the one he wants."
	node ifNil:[^nil].
	^ node children isEmpty
		ifTrue: [ nil ]
		ifFalse: [ 
			node children size = 1
				ifTrue: [node children at: 1]
				ifFalse: [  self  askForNodeSelectionFrom: node children]
		 ]
]

{ #category : #'stand out' }
NNavDirectionChild class >> findSelectionFor: navigationContext [
	^ self
		calculateBestSeletionFor: (self childNodeFor: navigationContext bestNodeForSelection)
		beforeSelection: navigationContext textSelection
		step: [ :selection :target | self selectionChildrenScope: selection for: target ]
]

{ #category : #'step into' }
NNavDirectionChild class >> selectionChildrenScope: selected for: node [
	| children |

	node ifNil: [ ^selected ].
	children := self childNodeFor: node.
	^children ifNil: [ selected ] ifNotNil: [children start to: children stop]
]
