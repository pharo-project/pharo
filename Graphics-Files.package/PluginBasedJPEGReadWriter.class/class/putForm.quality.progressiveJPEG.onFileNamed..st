image reading/writing
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName 
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."
	| writer |
	fileName asFileReference ensureDelete.
	writer := self on: (FileStream newFileNamed: fileName) binary.
	Cursor write showWhile: 
		[ writer 
			nextPutImage: aForm
			quality: quality
			progressiveJPEG: progressiveFlag ].
	writer close