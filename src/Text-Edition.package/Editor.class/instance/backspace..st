typing/selecting keys
backspace: aKeyboardEvent
	"Backspace over the last character."

	| startIndex |
	aKeyboardEvent shiftPressed ifTrue: [^ self backWord: aKeyboardEvent keyCharacter].
	self hasSelection
		ifTrue: [self replaceSelectionWith: self nullText]
		ifFalse: [startIndex := self markIndex + (self hasCaret ifTrue: [0] ifFalse: [1]).
			startIndex := 1 max: startIndex - 1.
			self backTo: startIndex].
	^false