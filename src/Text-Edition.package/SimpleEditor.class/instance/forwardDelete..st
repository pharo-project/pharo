typing - selecting keys
forwardDelete: aKeyboardEvent
	"Delete forward over the next character.
	  Make Undo work on the whole type-in, not just the one char.
	wod 11/3/1998: If there was a selection use #zapSelectionWith: rather than #backspace: which was 'one off' in deleting the selection. Handling of things like undo or typeIn area were not fully considered."

	| startIndex stopIndex |
	startIndex := self markIndex.
	startIndex > string size ifTrue: [
		^ false].
	self hasSelection ifTrue: [
		"there was a selection"
		self zapSelectionWith: ''.
		^ false].
	"Null selection - do the delete forward"
	beginTypeInIndex isNil	"no previous typing.  openTypeIn"
		ifTrue: [self openTypeIn].
	stopIndex := startIndex.
	(aKeyboardEvent keyValue = 127 and: [ aKeyboardEvent shiftPressed ])
		ifTrue: [ stopIndex := (self nextWord: stopIndex) - 1 ].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: ''.
	self selectFrom: startIndex to: startIndex-1.
	^false