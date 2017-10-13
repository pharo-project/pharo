initialize-release
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver.
	Must only be sent to a new instance; else we would need Object flushCache."
	self superclass: aClass.
	self setFormat: fmt.
	self methodDict: mDict.
	self traitComposition: nil