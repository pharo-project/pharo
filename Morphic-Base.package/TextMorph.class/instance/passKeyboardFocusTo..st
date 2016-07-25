editing
passKeyboardFocusTo: otherMorph 
	| w |
	self flag: #arNote.	"Do we need this?!"
	(w := self world) isNil 
		ifFalse: 
			[w 
				handsDo: [:h | h keyboardFocus == self ifTrue: [h newKeyboardFocus: otherMorph]]]