instance creation
externalNew
	"Create an instance of the receiver on the external heap"
	^self fromHandle: (ExternalAddress allocate: self structureSize)