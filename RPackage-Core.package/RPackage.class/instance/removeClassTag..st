class tags
removeClassTag: aSymbol
	"Remove the class tag from the receiver."
	
	self basicRemoveTag: (self 
		classTagNamed:  (self toTagName: aSymbol) 
		ifAbsent: [ ^ self ])