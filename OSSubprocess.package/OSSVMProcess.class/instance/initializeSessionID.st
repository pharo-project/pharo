initialize - release
initializeSessionID
	| oldFile | 
	self flag: #fixMe. 
	"grrr I need the sessionID (first int of the SQFile). .. there is no primitive to get it. Since it is the same for all files
	lets try to grab it from a file we know there will be always there: the image file."
	oldFile := StandardFileStream oldFileNamed: Smalltalk image imagePath.
	sessionID := oldFile basicFileID copy first: (self systemAccessor sizeOfInt).
	oldFile close.