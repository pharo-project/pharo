as yet unclassified
on: anObject getValue: getSel setValue: setSel min: min max: max quantum: quantum
	"Answer a new instance of the receiver with
	the given selectors as the interface."

	^self new
		min: min;
		max: max;
		quantum: quantum;
		on: anObject
		getValue: getSel
		setValue: setSel