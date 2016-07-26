actions
deleteFileOrDirectory
	"Delete the selected file or directory."

	|entry|
	self hasSelectedFileOrDirectory ifFalse: [^self].
	entry := self selectedFileEntry.
	entry isDirectory
		ifTrue: [(self 
					proceed: 'Are you sure you wish to delete the\selected directory along with its files?' withCRs translated
					title: 'Delete Directory' translated) ifTrue: [
						entry asFileReference delete.
						self
							clearEntryCache;
							updateDirectories]]
		ifFalse: [(self 
					proceed: 'Are you sure you wish to delete the\file' withCRs translated, ' "', entry name, '"?'
					title: 'Delete Directory' translated) ifTrue: [
						entry asFileReference delete.
						self
							selectedFileIndex: 0;
							clearEntryCache;
							updateFiles]].