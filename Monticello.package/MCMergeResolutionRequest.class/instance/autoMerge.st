actions
autoMerge
	"If there are not conflicts, merge, otherwise delegate to the UI"
	self hasConflicts
		ifTrue: [ self resolve ]
		ifFalse: [ self merge ]