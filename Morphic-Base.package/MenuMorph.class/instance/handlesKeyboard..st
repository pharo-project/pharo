keyboard control
handlesKeyboard: evt
	"Answer whether the receiver handles the keystroke represented by the event"

	^ evt anyModifierKeyPressed not or: [evt commandKeyPressed and: [self commandKeyHandler notNil]]