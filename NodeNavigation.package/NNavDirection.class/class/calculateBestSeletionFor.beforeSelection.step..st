private
calculateBestSeletionFor: node beforeSelection: selectionInText step: blockForStep
	| selectionInNode |
	
	node ifNil:[ ^selectionInText ].
	selectionInNode := node start to: node stop.
	^ (selectionInText = selectionInNode )
		ifTrue: [blockForStep value: selectionInText  value: node ]
		ifFalse: [selectionInNode ].