instance creation
newOnDirectory: aFileReference baseLabel: aTitle contents: someContents
	
	^ self new
			directory: aFileReference;
			baseLabel: aTitle;
			contents: someContents;
			yourself