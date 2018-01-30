Extension { #name : #MorphicUIManager }

{ #category : #'*Files-Prompt' }
MorphicUIManager >> fileDoesNotExistUserHandling: filename [
	| selection newName |
	selection := self
		chooseFrom:
			{('create a new file' translated).
			('choose another name' translated).
			('cancel' translated)}
		title: filename, ' does not exist.'.
	self halt.
	selection = 1
		ifTrue: [ ^ File openForWriteFileNamed: filename ].
	selection = 2
		ifFalse: [ ^ self ].
	newName := self request: 'Enter a new file name' initialAnswer: filename.
	^ File openForWriteFileNamed: filename
]

{ #category : #'*Files-Prompt' }
MorphicUIManager >> fileDoesNotExistsDefaultAction: anException [

	^ anException readOnly
		ifTrue: [ self readOnlyFileDoesNotExistUserHandling: anException fileName]
		ifFalse: [self fileDoesNotExistUserHandling: anException fileName]

]

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

{ #category : #'*Files-Prompt' }
MorphicUIManager >> readOnlyFileDoesNotExistUserHandling: filename [

	| files choices selection newName directory |
	
	directory := filename asFileReference parent.
	files := directory fileNames.
	
	choices := filename correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	
	selection := self chooseFrom: choices lines: (Array with: 5)
		title: filename, ' does not exist.'.
		
	selection = choices size ifTrue:[
		"cancel"
		"should we raise another exception here?"
		^ nil ].
	
	selection < (choices size - 1) ifTrue: [
		newName := directory / (choices at: selection)].
	
	selection = (choices size - 1) ifTrue: [
		newName := directory / (self
				request: 'Enter a new file name' 
				initialAnswer: filename) ].

	newName ifNotNil: [ ^ newName readStream ].
	
	^ self error: 'Could not open a file'
]
