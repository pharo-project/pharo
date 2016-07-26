controls
newButtonLabel: aString
	"Answer a new button text label."

	^self
		newButtonLabelFor: nil
		label: aString
		getEnabled: nil