copying
, aRunArray 
	"Answer a new RunArray that is a concatenation of the receiver and
	aRunArray."

	| new newRuns |
	(aRunArray isMemberOf: RunArray)
		ifFalse: 
			[new := self copy.
			"attempt to be sociable"
			aRunArray do: [:each | new addLast: each].
			^new].
	runs size = 0 ifTrue: [^aRunArray copy].
	aRunArray runs size = 0 ifTrue: [^self copy].
	(values at: values size) ~= (aRunArray values at: 1)
		ifTrue: [^RunArray
					runs: runs , aRunArray runs
					values: values , aRunArray values].
	newRuns := runs
			copyReplaceFrom: runs size
			to: runs size
			with: aRunArray runs.
	newRuns at: runs size put: (runs at: runs size) + (aRunArray runs at: 1).
	^RunArray
		runs: newRuns
		values: 
			(values
				copyReplaceFrom: values size
				to: values size
				with: aRunArray values)