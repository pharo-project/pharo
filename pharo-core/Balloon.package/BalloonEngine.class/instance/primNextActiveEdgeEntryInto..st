primitives-incremental
primNextActiveEdgeEntryInto: edgeEntry
	"Store the next entry of the AET at the current y-value in edgeEntry.
	Return false if there is no entry, true otherwise."
	<primitive: 'primitiveNextActiveEdgeEntry' module: 'B2DPlugin'>
	^self primitiveFailed