event handling
handlesKeyboard: evt
	"True when either the filter morph doesn't have the focus and the key
	is a text key or backspace or no menus have the focus and is up or down arrow."
	
	^(super handlesKeyboard: evt) or: [
		(self choiceMenus anySatisfy: [:m | m hasKeyboardFocus])
			ifTrue: [evt keyCharacter = Character backspace
						or: [evt keyCharacter > Character space
						or: [evt keyCharacter = Character cr
						or: [evt keyCharacter = Character arrowLeft
						or: [evt keyCharacter = Character arrowRight]]]]]
			ifFalse: [evt keyCharacter = Character arrowUp
						or: [evt keyCharacter = Character arrowDown
						or: [self filterMorph hasKeyboardFocus not]]]]