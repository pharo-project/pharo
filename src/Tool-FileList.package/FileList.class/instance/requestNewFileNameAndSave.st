private
requestNewFileNameAndSave
	| newName |
	newName := UIManager default request: 'Enter a new file name' initialAnswer: reference fullName.
	newName ifNil: [ ^ self].
	reference := newName asFileReference.
	(reference asFileReference exists) ifTrue:[ ^ self updateFileContents ].
	self put: self sourceTextModel getText asString.
	self updateFileList