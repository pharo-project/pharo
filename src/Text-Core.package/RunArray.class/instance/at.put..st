accessing
at: index put: aValue
	"Set an element of the RunArray"

	| runIndex offsetInRun lastValue runLength runReplacement valueReplacement iStart iStop |
	index isInteger
		ifFalse: [ self errorNonIntegerIndex ].
	(index between: 1 and: self size)
		ifFalse: [ self errorSubscriptBounds: index ].
	self
		at: index
		setRunOffsetAndValue: [ :run :offset :value | 
			runIndex := run.
			offsetInRun := offset.
			lastValue := value ].
	aValue = lastValue
		ifTrue: [ ^ aValue ].
	runLength := runs at: runIndex.
	runReplacement := Array with: offsetInRun with: 1 with: runLength - offsetInRun - 1.
	valueReplacement := Array with: lastValue with: aValue with: lastValue.
	iStart := offsetInRun = 0
		ifTrue: [ 2 ]
		ifFalse: [ 1 ].
	iStop := offsetInRun = (runLength - 1)
		ifTrue: [ 2 ]
		ifFalse: [ 3 ].
	self
		setRuns: (runs copyReplaceFrom: runIndex to: runIndex with: (runReplacement copyFrom: iStart to: iStop))
		setValues: (values copyReplaceFrom: runIndex to: runIndex with: (valueReplacement copyFrom: iStart to: iStop)).
	self coalesce.
	^ aValue