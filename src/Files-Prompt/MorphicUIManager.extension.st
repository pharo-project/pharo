Extension { #name : #MorphicUIManager }

{ #category : #'*Files-Prompt' }
MorphicUIManager >> fileDoesNotExistUserHandling: anException [
	| selection newName filename |

	filename := anException fileName.
	selection := self
		chooseFrom:
			{('Force recursive file creation' translated).
			('Choose another file name' translated).
			('Cancel' translated)}
		title: filename, ' does not exist.'.
		
	"If user presses escape, selection will be 0.
	If user selects cancel, selection will be the last choice"
	(#( 0 3 ) includes: selection) ifTrue:[
		Abort signal: 'Please close this to abort file opening' ].
	
	"Create a new file.
	We depend on FileSystem here to recursively create the directory for the file."
	selection = 1
		ifTrue: [
			filename asFileReference ensureCreateFile.
			^ File openForWriteFileNamed: filename ].
		
	"Create a new file"
	newName := self request: 'Enter a new file name' initialAnswer: filename.
	^ File openForWriteFileNamed: newName
]

{ #category : #'*Files-Prompt' }
MorphicUIManager >> fileDoesNotExistsDefaultAction: anException [

	^ anException readOnly
		ifTrue: [ self readOnlyFileDoesNotExistUserHandling: anException]
		ifFalse: [self fileDoesNotExistUserHandling: anException]

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
MorphicUIManager >> readOnlyFileDoesNotExistUserHandling: anException [

	| files choices selection newName directory filename |

	filename := anException fileName.	
	directory := filename asFileReference parent.
	files := directory fileNames.
	
	choices := filename correctAgainst: files.
	choices add: 'Choose another file name'.
	choices add: 'Cancel'.
	
	selection := self chooseFrom: choices lines: (Array with: 5)
		title: filename, ' does not exist.'.
	
	"If user presses escape, selection will be 0.
	If user selects cancel, selection will be the last choice"
	(selection = 0 or: [selection = choices size]) ifTrue:[
		Abort signal: 'Please close this to abort file opening' ].
	
	selection < (choices size - 1) ifTrue: [
		newName := directory / (choices at: selection)].
	
	selection = (choices size - 1) ifTrue: [
		newName := directory / (self
				request: 'Enter a new file name' 
				initialAnswer: filename) ].

	newName ifNotNil: [ ^ newName readStream ].
	
	^ self error: 'Could not open a file'
]
