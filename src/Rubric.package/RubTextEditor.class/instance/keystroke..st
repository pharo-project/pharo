typing support
keystroke: aKeyboardEvent
	textArea announce: ((RubKeystroke with: aKeyboardEvent) morph: self).
	(self dispatch: aKeyboardEvent)
		ifTrue: [ "If not a normal character"
			self doneTyping.
			self storeSelectionInText.
			^ self ].
	"normal character"
	self openTypeIn.
	self hasSelection
		ifTrue: [ 
			"save highlighted characters"
			self editingState previousInterval: self selectionInterval].
	"Notice selection changed"
	self unselect.
	self storeSelectionInText