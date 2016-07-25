actions
save
	self page content isEmpty
		ifTrue: [ self deleteFromFileSystem ]
		ifFalse: [ self writeToFileSystem ]