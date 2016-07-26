testing
longOptionAt: aString ifAbsent: absentBlock
	| optionStart |
	optionStart := '--' , aString , '='.
	^ self arguments
		detect: [ :arg | arg beginsWith: optionStart ]
		ifFound: [ :option | (option splitOn: '=') second ]
		ifNone: absentBlock