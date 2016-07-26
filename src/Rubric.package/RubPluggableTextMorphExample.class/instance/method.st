accessing
method
	^ [ self selectedClass >> (self selectedMethodName getString ifEmpty: [ ^nil ]) asSymbol ] on: Error do: [  ]