initialize-release
fileName: anObject
	"Initialize me to write to a file stream described by anObject.
	anObject is either a String naming a file or the special #stdout value
	for using the standard output stream of the virtual machine process."
	
	self close.
	fileName := anObject