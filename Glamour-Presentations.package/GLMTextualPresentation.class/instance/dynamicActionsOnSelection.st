accessing
dynamicActionsOnSelection
	"Unlike the other types of presentations, in a text presentation the dynamic actions should be computed even if the selection port is not explicitly populated. A text presentation uses the port #selectedText to transmit the text that was selected by the user, and not the #selection port"
	(selectionDynamicActionsBlock isNil or: [ 
		self selectedText isNil ])
		ifTrue: [ ^ OrderedCollection new ].
	^ selectionDynamicActionsBlock glamourValue: self