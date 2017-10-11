accessing
fileName
	"The file name that I will write to.
	Lazy initialized to a default.
	When I connect to the standard output stream of the 
	virtual machine process I return #stdout."
	
	^ fileName ifNil: [ fileName := self class defaultLogFileName ]