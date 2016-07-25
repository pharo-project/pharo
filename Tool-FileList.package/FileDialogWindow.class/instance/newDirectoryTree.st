instance creation
newDirectoryTree
	"Answer a new directory tree."
	
	^(self newTreeFor: self
		list: #directories
		selected: #selectedDirectory
		changeSelected: #selectedDirectory:)
		minHeight: 200;
		minWidth: 180