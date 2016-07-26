drawing
adornmentColor
	"color to Indicate edit status for the given morph."

	self hasValidText
		ifFalse: [ ^ Color red ].
	^ super adornmentColor