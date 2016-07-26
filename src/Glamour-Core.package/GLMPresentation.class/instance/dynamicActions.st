accessing
dynamicActions
	"These are actions that make sense for the entire presentation.
	They are dynamic in the sense that they will be evaluated lazily by the renderer"
	dynamicActionsBlock isNil ifTrue: [ ^ OrderedCollection new ].
	^ dynamicActionsBlock glamourValue: self