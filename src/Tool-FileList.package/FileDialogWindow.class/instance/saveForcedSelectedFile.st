actions
saveForcedSelectedFile
	"Open a stream on the selected file if available and return it."

	|d f|
	d := self selectedFileDirectory ifNil: [^nil].
	f := self selectedFileName ifNil: [self fileNameText trimBoth].
	f ifEmpty: [^nil].
	^d forceNewFileNamed: f