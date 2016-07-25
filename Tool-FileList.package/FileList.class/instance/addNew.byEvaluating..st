file menu action
addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| response newName index |
	self okToChange ifFalse: [^ self].
	(response := UIManager default
						request: ('New {1} Name?' translated format: {aString translated})
						initialAnswer: ('{1}Name' translated format: {aString translated}))
		isEmptyOrNil ifTrue: [^ self].
	newName := response asFileName.
	Cursor wait showWhile: [
		aBlock value: newName].
	self updateFileList.
	self updateDirectory.
	list ifNotNil:[
	index := list indexOf: newName.
	index = 0 ifTrue: [
		index := list findFirst: [:line | line basename endsWith: newName]].
	self fileListIndex: index.]
