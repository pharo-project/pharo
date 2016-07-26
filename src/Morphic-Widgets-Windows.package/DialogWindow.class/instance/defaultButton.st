actions
defaultButton
	"Answer the default button."

	^self
		findDeepSubmorphThat: [:m |
			(m isKindOf: PluggableButtonMorph) and: [m isDefault]]
		ifAbsent: [] 