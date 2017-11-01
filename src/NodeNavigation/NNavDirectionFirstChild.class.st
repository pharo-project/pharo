"
I navigate throw the childs picking allways the firs one
"
Class {
	#name : #NNavDirectionFirstChild,
	#superclass : #NNavDirectionChild,
	#category : 'NodeNavigation-Core'
}

{ #category : #builder }
NNavDirectionFirstChild class >> buildShortcut: aBuilder [
	<keymap>
	(aBuilder shortcut: #firstChild) 
		category: #SmalltalkEditor
		default: self defaultKeyCombination
		do: [ :morph | morph standOutIntoFirstScope]
		description: 'Select the node scope reducing to the first child'



]

{ #category : #private }
NNavDirectionFirstChild class >> childNodeFor: node [
	"We find the child for a node, if it has not childs nil, if has more than one return the first one."
	node ifNil:[^nil].
	^ node children isEmpty
		ifTrue: [ nil ]
		ifFalse: [ node children at: 1]

]

{ #category : #builder }
NNavDirectionFirstChild class >> defaultKeyCombination [
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [ Character arrowRight meta shift ]
		ifFalse: [ $o meta shift  ]


]
