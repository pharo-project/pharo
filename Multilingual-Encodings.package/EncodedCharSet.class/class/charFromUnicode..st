class methods
charFromUnicode: unicode

	| table index |
	unicode < 128 ifTrue: [^ Character value: unicode].

	table := self ucsTable.
	index := table indexOf: unicode.
	index = 0 ifTrue: [
		^ nil.
	].

	^ Character leadingChar: self leadingChar code: index - 1.

