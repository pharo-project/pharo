accessing
next

	| n |
	n := self converter nextFromStream: self.
	n ifNil: [^ nil].
	isBinary and: [n isCharacter ifTrue: [^ n asciiValue]].
	^ n.
