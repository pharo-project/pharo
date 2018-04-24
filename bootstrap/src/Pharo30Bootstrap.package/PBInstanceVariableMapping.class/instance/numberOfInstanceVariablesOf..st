initialize
numberOfInstanceVariablesOf: aClassName 
	^ (environment classNamed: aClassName) allSlots size