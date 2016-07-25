actions
newMainPanel
	"Answer a new main panel."
	| panel |
	panel := self newDialogPanel.
	self newContentMorph ifNotNil: [:m | panel 	addMorphBack: m ].
	self newButtonRow ifNotNil: [:m | panel 	addMorphBack: m ].
	^ panel