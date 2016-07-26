events
keyStroke: evt
	"Handle tabbing and arrows and cr/space."
	
	|char selectable|
	(self navigationKey: evt) ifTrue: [^self].
	char := evt keyCharacter.
	char = Character space 
		ifTrue: 
			[selectedItem ifNotNil: 
					[selectedItem hasSubMenu 
						ifTrue: 
							[evt hand newMouseFocus: selectedItem subMenu.
							^selectedItem subMenu takeKeyboardFocus]
						ifFalse: 
							[^selectedItem invokeWithEvent: evt]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: evt].
			^self].
	(char = Character arrowLeft or: [char = Character arrowRight]) ifTrue: [
		(selectedItem notNil and: [selectedItem hasSubMenu]) ifTrue: [
			evt hand newMouseFocus: selectedItem subMenu.
			selectedItem subMenu moveSelectionDown: 1 event: evt.
			^evt hand newKeyboardFocus: selectedItem subMenu]].
	char = Character arrowUp ifTrue: [^self moveSelectionDown: -1 event: evt].	"up arrow key"
	char = Character arrowDown ifTrue: [^self moveSelectionDown: 1 event: evt].	"down arrow key"
	char = Character pageUp ifTrue: [^self moveSelectionDown: -5 event: evt].	"page up key"
	char = Character pageDown ifTrue: [^self moveSelectionDown: 5 event: evt].	"page down key"
