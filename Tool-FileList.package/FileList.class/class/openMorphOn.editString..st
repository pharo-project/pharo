morphic ui
openMorphOn: aFileStream editString: editString 
	"Open a morphic view of a FileList on the given file."
	^ (self new setFileStream: aFileStream)
		contents: editString;
		morphicViewOnFile: aFileStream fullName asFileReference