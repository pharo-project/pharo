accessing
shortcut 
	"Shortcut character that this action should be triggered with when the focus rests on the corresponding presentation. 
	Modifiers are platform specific an can not be defined." 
	 
	^ self keymap 
		ifNotNil: [ self keymap currentCharacter ]