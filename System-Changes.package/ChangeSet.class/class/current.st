current changeset
current
	"return the current changeset assure first that we have a named changeset."
	
	(current isNil or: [current isMoribund])
		ifTrue: [self newChanges: (self assuredChangeSetNamed: 'Unnamed') withOld: current] .
	^ current