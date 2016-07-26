adding
addLast: value
	"Add value as the last element of the receiver."
	lastIndex := nil. "flush access cache"
	(runs size=0 or: [values last ~= value])
		ifTrue:
			[runs := runs copyWith: 1.
			values := values copyWith: value]
		ifFalse:
			[runs at: runs size put: runs last+1].
	^value