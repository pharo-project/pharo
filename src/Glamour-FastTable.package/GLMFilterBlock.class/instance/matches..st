testing
matches: anObject
	"Return true if anObject should be displayed; false otherwise."
	self filterBlock ifNil: [ ^ true ].
	^ self filterBlock valueWithEnoughArguments: { anObject. pattern }