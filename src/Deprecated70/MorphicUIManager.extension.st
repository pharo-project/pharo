Extension { #name : #MorphicUIManager }

{ #category : #'*Deprecated70' }
MorphicUIManager >> fileDoesNotExistUserHandling: anException [
	| selection newName filename |

	self 
		deprecated: 'Deprecated since file exceptions should not rely on a UI being present' 
		on: '20 Jun 2018' 
		in: 'Pharo70'.
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

{ #category : #'*Deprecated70' }
MorphicUIManager >> fileDoesNotExistsDefaultAction: anException [

	self 
		deprecated: 'Deprecated since file exceptions should not rely on a UI being present' 
		on: '20 Jun 2018' 
		in: 'Pharo70'.
	^ anException readOnly
		ifTrue: [ self readOnlyFileDoesNotExistUserHandling: anException]
		ifFalse: [self fileDoesNotExistUserHandling: anException]

]

{ #category : #'*Deprecated70' }
MorphicUIManager >> readOnlyFileDoesNotExistUserHandling: anException [

	| files choices selection newName directory filename |

	self 
		deprecated: 'Deprecated since file exceptions should not rely on a UI being present' 
		on: '20 Jun 2018' 
		in: 'Pharo70'.
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
