drawing
adornmentColor
	"color to Indicate edit status for the given morph."

	self hasEditingConflicts
		ifTrue: [ ^ Color red ].
	self hasUnacceptedEdits
		ifTrue: [ ^ Color orange ].
	^ Color transparent