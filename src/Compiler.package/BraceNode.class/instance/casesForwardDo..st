enumerating
casesForwardDo: aBlock
	"For each case in forward order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	1 to: (numCases := elements size) do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]