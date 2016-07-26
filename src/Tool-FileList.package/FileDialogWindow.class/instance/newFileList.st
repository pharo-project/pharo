instance creation
newFileList
	"Answer a new file list."
	
	^(self
			newListFor: self
			list: #files
			selected: #selectedFileIndex
			changeSelected: #selectedFileIndex:
			icon: #iconFor:
			getEnabled: nil
			help: nil)
		wrapSelector: #basename;
		doubleClickSelector: #doubleClickFile;
		minWidth: 200