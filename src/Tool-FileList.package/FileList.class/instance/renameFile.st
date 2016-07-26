file menu action
renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	(response := UIManager default request: 'NewFileName?' translated
 					initialAnswer: reference basename)
		isEmptyOrNil ifTrue: [^ self].
	newName := response asFileName.
	newName = reference basename ifTrue: [^ self].
	reference renameTo: newName.
	self updateFileList.
	listIndex := list findFirst: [:item | item = newName].
	
	listIndex > 0 
		ifTrue: [reference := newName].
	self changed: #fileListIndex.
