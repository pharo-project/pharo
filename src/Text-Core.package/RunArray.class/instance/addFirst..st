adding
addFirst: value
	"Add value as the first element of the receiver."
	lastIndex := nil. "flush access cache"
	(runs size=0 or: [values first ~= value])
		ifTrue:
			[runs := {1}, runs.
			values := {value}, values]
		ifFalse:
			[runs at: 1 put: runs first+1]