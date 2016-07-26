border-styles-buttons
radioButtonCornerStyleFor: aRadioButton
	"Answer the corner style for radio buttons.
	Answer square when not disabled since the form is rounded."
	
	^aRadioButton enabled
		ifTrue: [#square]
		ifFalse: [#rounded]