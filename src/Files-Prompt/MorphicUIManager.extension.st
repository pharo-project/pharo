Extension { #name : #MorphicUIManager }

{ #category : #'*Files-Prompt' }
MorphicUIManager >> fileExistsDefaultAction: anException [
	| file choice |
	
	file := anException file.
	choice := self chooseFrom: {
		'overwrite that file'.
		'choose another name'.
		'cancel'
	} title: file name, ' already exists.'.
 
	choice = 1 ifTrue: [
		file delete.
		file exists ifTrue:  [ self error: 'Could not delete the old version of that file' ].
		^ file ].

	choice = 2 ifTrue: [ | newName newFile |
		newName := self request: 'Enter a new file name' initialAnswer: file name.
		newName ifNil: [ Abort signal ].
		newFile := File named: newName.
		^ newFile checkDoesNotExist ].

	Abort signal: 'Please close this to abort file opening'
]
