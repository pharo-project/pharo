error handling
fileExistsUserHandling: fullFileName
	| file localName choice newName newFullFileName |
	
	file := fullFileName asFileReference.
	localName := file basename.
	choice := UIManager default chooseFrom:{
		'overwrite that file'.
		'choose another name'.
		'cancel'
	} title: localName, ' already exists.'.
 
	choice = 1 ifTrue: [
		file delete.
		file exists ifTrue:  [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName := UIManager default request: 'Enter a new file name' initialAnswer: fullFileName.
		newName ifNil: [Abort signal].
		newFullFileName := self fullName: newName.
		^ self newFileNamed: newFullFileName].

	Abort signal: 'Please close this to abort file opening'