private
selectedStatements
	selectedNode isMessage ifTrue: [ ^{selectedNode} ].
	^(selectedNode statements select: [ :stat | stat containedBy: selectionPreviousHighligth ]).