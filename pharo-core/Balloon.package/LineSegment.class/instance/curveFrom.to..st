vector functions
curveFrom: parameter1 to: parameter2
	"Create a new segment like the receiver but starting/ending at the given parametric values"
	| delta |
	delta := end - start.
	^self shallowCopy from: delta * parameter1 + start to: delta * parameter2 + start