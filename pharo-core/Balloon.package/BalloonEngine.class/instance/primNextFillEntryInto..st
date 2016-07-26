primitives-incremental
primNextFillEntryInto: fillEntry
	"Store the next fill entry of the active edge table in fillEntry.
	Return false if there is no such entry, true otherwise"
	<primitive: 'primitiveNextFillEntry' module: 'B2DPlugin'>
	^self primitiveFailed