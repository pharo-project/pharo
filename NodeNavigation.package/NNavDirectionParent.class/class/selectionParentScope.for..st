step over
selectionParentScope: selected for: node
	| nodeSelection |

	node ifNil: [ ^selected ].
	nodeSelection := node start to: node stop.
	selected = nodeSelection
		ifTrue: [ ^self selectionParentScope: selected for: node parent  ]
		ifFalse: [ ^nodeSelection ].