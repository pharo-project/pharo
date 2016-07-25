actions
useInspectorAt: index
	"choose an inspector at the given index from the given inspectorTypes.
	If the index is out of bounds, simply crop it to the neares value"
	| types |
	types := self inspectorTypes.
	self inspectorType: (types at: (index min: types size max: 1))