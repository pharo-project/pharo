file menu action
deleteFile
	"Delete the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	
	(self confirm: ('Really delete {1}?' translated format: { reference basename } ))
		ifFalse: [^ self].
	
	reference delete.
	reference := reference parent.
	
	self updateFileList.
	brevityState := #FileList.
	self get