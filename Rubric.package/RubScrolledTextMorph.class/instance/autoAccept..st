accessing
autoAccept: aBoolean
	"Whether I should accept my contents on each change."
	autoAccept := aBoolean.
	aBoolean
		ifTrue: [ self withoutAdornment; acceptContents]
		ifFalse: [ self withAdornment ]