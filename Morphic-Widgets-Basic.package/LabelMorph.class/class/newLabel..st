instance creation
newLabel: aString
	"Answer a new text label morph."

	^self
		newLabelFor: nil
		label: aString
		getEnabled: nil