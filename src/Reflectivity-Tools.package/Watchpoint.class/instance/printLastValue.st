printing
printLastValue
	values ifNil: [ ^''].
	values ifEmpty: [ ^'not run' ].
	^values last value printStringLimitedTo: 5.