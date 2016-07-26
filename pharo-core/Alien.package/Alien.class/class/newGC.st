instance creation
newGC
	"Answer a new instance of the default size whose data resides on the external C heap,
	 and whose data will be reclaimed automatically when the instance is garbage collected."
	^self newGC: self dataSize