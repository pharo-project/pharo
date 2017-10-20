defaults
default: defaultOption
	"Set the default button."

	defaultOption
		ifNil: [self cancelButton isDefault: true]
		ifNotNil: [defaultOption
					ifTrue: [self yesButton isDefault: true]
					ifFalse: [self noButton isDefault: true]]