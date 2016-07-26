delegated
exists
	"Return true if the file reference exist (e.g., if there is a file on the hard disk pointed by the file reference)
	
	E.g., (if you are on Unix or OSX)
	'/tmp/' asFileReference exists => true
	'/tmp/zorkbar' asFileReference exists => false
	"
	^ self resolve exists