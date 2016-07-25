private
selectedTemporaryVariables
	selectedNode isSequence ifFalse: [ ^#() ].
	selectedNode temporaries ifEmpty: [^OrderedCollection new].
	^ ((selectedNode temporaries at: 1) containedBy: selectionPreviousHighligth )
		ifTrue: [ selectedNode temporaries ]
		ifFalse: [#()].
	