initialize-release
superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	self subclasses: nil. 