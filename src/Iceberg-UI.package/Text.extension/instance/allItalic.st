*Iceberg-UI
allItalic
	"Force this whole text to be italic."
	string isEmpty ifTrue: [ ^ self ].
	^ self addAttribute: TextEmphasis italic from: 1 to: string size.
