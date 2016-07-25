accessing
contents
	"Answer contents of the pipe, and return the contents to the pipe so it can still be read."

	| string |
	self closed ifTrue:
		[self notify: self printString, ' ', self reader printString, ' closed'.
		^ nil].
	string := self reader upToEnd.
	string isEmpty ifFalse:
		[self writer closed
			ifTrue: [self notify: self printString, ' ', self writer printString,
								' closed, cannot replace contents']
			ifFalse: [self nextPutAll: string]].
	^ string