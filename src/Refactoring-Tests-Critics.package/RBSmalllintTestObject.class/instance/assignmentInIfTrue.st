methods
assignmentInIfTrue
	| variable |
	self isVariable 
		ifTrue: [variable := self]
		ifFalse: [variable := self printString].
	^variable