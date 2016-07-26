accessing
selectedDirectory: anObject
	"Set the value of selectedDirectory"

	selectedDirectory := anObject.
	self
		selectedFileIndex: 0;
		updateSelectedDirectory;
		updateFiles