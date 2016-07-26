nonediting/nontyping keys
cursorEnd: aKeyboardEvent 

	"Private - Move cursor end of current line."
	| string |
	self closeTypeIn.
	string := self string.
	self
		moveCursor:
			[:position | self walkAlongDisplayedLine
				ifTrue:[| targetLine |
					targetLine := self paragraph lines at: (self paragraph lineIndexOfCharacterIndex: position).
					targetLine = self paragraph lines  last 
						ifTrue:[targetLine last + 1]
						ifFalse:[targetLine last]]
				ifFalse:[
					string
						indexOf: Character cr
						startingAt: position
						ifAbsent:[string size + 1]]]
		forward: true
		specialBlock:[:dummy | string size + 1]
		event: aKeyboardEvent.
	^true