*CodeExport
fileOutCategory: category on: aFileStream 
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."
	^self fileOutCategory: category on: aFileStream initializing: true