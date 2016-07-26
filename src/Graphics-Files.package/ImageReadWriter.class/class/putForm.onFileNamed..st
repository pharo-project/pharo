image reading/writing
putForm: aForm onFileNamed: fileName 
	"Store the given form on a file of the given name."
	| writer |
	writer := self on: (FileStream newFileNamed: fileName) binary.
	Cursor write showWhile: [ writer nextPutImage: aForm ].
	writer close