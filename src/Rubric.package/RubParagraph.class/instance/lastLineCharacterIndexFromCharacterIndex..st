querying
lastLineCharacterIndexFromCharacterIndex: anIndex
	| targetLine |
	targetLine := self lines at: (self lineIndexOfCharacterIndex: anIndex).
	^ targetLine = self lines last
		ifTrue: [ targetLine last + 1 ]
		ifFalse: [ targetLine last ]