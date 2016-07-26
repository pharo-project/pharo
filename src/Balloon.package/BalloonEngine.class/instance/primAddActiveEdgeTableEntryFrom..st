primitives-incremental
primAddActiveEdgeTableEntryFrom: edgeEntry
	"Add edge entry to the AET."
	<primitive: 'primitiveAddActiveEdgeEntry' module: 'B2DPlugin'>
	(self canProceedAfter: self primGetFailureReason) ifTrue:[
		^self primAddActiveEdgeTableEntryFrom: edgeEntry
	].
	^self primitiveFailed