error handling
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	
	dir := fullFileName asFileReference.
	files := dir parent fileNames.
	fileName := dir basename.
	
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	
	selection := UIManager default chooseFrom: choices lines: (Array with: 5)
		title: (fullFileName asFileReference basename), ' does not exist.'.
		
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName := dir parent / (choices at: selection)].
	selection = (choices size - 1) ifTrue: [
		newName := UIManager default 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ self readOnlyFileNamed: (self fullName: newName)].
	^ self error: 'Could not open a file'