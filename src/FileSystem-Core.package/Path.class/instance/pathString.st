printing
pathString
	"Return a string containing the path elements of the receiver, without the 'Path *' part"
	"
((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') relativeToReference: FileSystem workingDirectory) pathString
	>>> 'book-result/W01-Welcome'"
	^ String streamContents: [ :str| 
			self printPathOn: str delimiter: self delimiter ]