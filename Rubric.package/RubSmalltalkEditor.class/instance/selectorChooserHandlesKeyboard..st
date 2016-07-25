basic-completion
selectorChooserHandlesKeyboard: aKeyboardEvent
	"Return true or false on whether the code editor can handle keyboard event"
	^ (selectorChooser notNil and: [ selectorChooser handlesKeyboard: aKeyboardEvent ])
