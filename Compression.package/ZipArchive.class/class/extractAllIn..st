fileIn/Out
extractAllIn: aFileName
	"Service method to extract all contents of a zip."
	| directory |
	directory := (UIManager default chooseDirectoryFrom: aFileName asFileReference) ifNil: [^ self].
	^ (self new) 
		readFrom: aFileName;
		extractAllTo: directory.