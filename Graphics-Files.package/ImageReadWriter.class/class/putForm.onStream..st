image reading/writing
putForm: aForm onStream: aWriteStream 
	"Store the given form on a file of the given name."
	| writer |
	writer := self on: aWriteStream.
	Cursor write showWhile: [ writer nextPutImage: aForm ].
	writer close