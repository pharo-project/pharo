events
keyStroke: evt
	"Handle keboard item matching."
	
	| matchString char asc selectable help |
	help := self theme builder newBalloonHelp: 'Enter text to\narrow selection down\to matching items ' withCRs for: self corner: #topLeft.
	help popUpForHand: self activeHand.
	(self rootMenu hasProperty: #hasUsedKeyboard) 
		ifFalse: 
			[self rootMenu setProperty: #hasUsedKeyboard toValue: true.
			self changed].
	(evt commandKeyPressed and: [self commandKeyHandler notNil]) 
		ifTrue: 
			[self commandKeyHandler commandKeyTypedIntoMenu: evt.
			^self deleteIfPopUp: evt].
	char := evt keyCharacter.
	asc := char asciiValue.
	char = Character cr 
		ifTrue: 
			[selectedItem ifNotNil: 
					[selectedItem hasSubMenu 
						ifTrue: 
							[evt hand newMouseFocus: selectedItem subMenu.
							^evt hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: 
							["self delete."

							^selectedItem invokeWithEvent: evt]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: evt].
			^self].
	asc = 27 
		ifTrue: 
			["escape key"

			self valueOfProperty: #matchString
				ifPresentDo: 
					[:str | 
					str isEmpty 
						ifFalse: 
							["If filtered, first ESC removes filter"

							self setProperty: #matchString toValue: String new.
							self selectItem: nil event: evt.
							^self displayFiltered: evt]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			^ self deselectAndFocusOutermenuOn: evt].
		
	"Left arrow key - If we are in a submenu, then we remove myself (i.e., the current morph) and move the focus to the owner popup"
	(asc = 28)
		ifTrue: [ 			
				"If a stand-alone menu, do nothing"
				popUpOwner ifNil: [^self].
				"If a sub-menu, then deselect, and return focus to outer menu"
				^ self deselectAndFocusOutermenuOn: evt].
	
	"Right arrow key - If the selected menu item has a submenu, then we move the focus to the submenu "
	(asc = 29) 
		ifTrue: 
			["right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: 
					[evt hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: evt.
					^evt hand newKeyboardFocus: selectedItem subMenu]].
			
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: evt].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: evt].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: evt].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: evt].	"page down key"
		
	"If we reach this point, it means that we are editing the filter associated to each menu. "
	"In case ther eis no filter associated to the menu, we simply create one"
	matchString := self valueOfProperty: #matchString ifAbsentPut: [ String new ].

	"If we press the backspace, then we simply remove the last character from matchString"
	(char = Character backspace and: [ matchString notEmpty ])
		ifTrue: [ matchString := matchString allButLast. 
				   self recordFiltering: matchString.
				   self displayFiltered: evt. ].
	
	"No need to go further if the character is not alphanumeric, i.e., not useful for filtering"
	char isAlphaNumeric ifFalse: [ ^ self ].
	matchString := matchString, char asString.
		
	self recordFiltering: matchString.
	self displayFiltered: evt.

