"
Go back to the brother before
"
Class {
	#name : #NNavDirectionPreviousBrother,
	#superclass : #NNavDirectionSibling,
	#category : #'NodeNavigation-Core'
}

{ #category : #'step over' }
NNavDirectionPreviousBrother class >> brotherPositionFor: nodePosition brothers: brothers [
	^ nodePosition = 1 ifTrue:  [ brothers size ] ifFalse: [ nodePosition - 1 ].
]

{ #category : #builder }
NNavDirectionPreviousBrother class >> buildShortcut: aBuilder [
	<keymap>

	(aBuilder shortcut: #previousSibling) 
		category: #SmalltalkEditor 
		default: self defaultKeyCombination 
		do: [ :morph | morph standOutPreviousChild]
		description: 'Select the node scope reducing to the previous sibling'



]

{ #category : #builder }
NNavDirectionPreviousBrother class >> defaultKeyCombination [
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [Character arrowLeft meta  ]
		ifFalse: [ $u meta  ]

]
