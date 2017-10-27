*Files-Prompt
readOnlyFileDoesNotExistUserHandling: filename

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