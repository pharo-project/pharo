queries
allMethodsAccessingSlot: aSlot
	"return all methods that access aSlot in this class and subclasses"
	^self withAllSubclasses flatCollect: [ :class |
		class methodsAccessingSlot: aSlot.
	]