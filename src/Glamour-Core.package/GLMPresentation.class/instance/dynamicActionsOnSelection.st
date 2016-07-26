accessing
dynamicActionsOnSelection
	"These are actions that make sense only in the context of values held in the selection port.
	They are dynamic in the sense that they will be evaluated lazily by the renderer"
	(selectionDynamicActionsBlock isNil or: [ 
		self selection isNil ])
		ifTrue: [ ^ OrderedCollection new ].
	^ selectionDynamicActionsBlock glamourValue: self