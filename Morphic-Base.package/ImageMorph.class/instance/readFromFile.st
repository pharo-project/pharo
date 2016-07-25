menu commands
readFromFile
	| fileName |
	fileName := UIManager default
		request: 'Please enter the image file name' translated
		initialAnswer: 'fileName'.
	fileName isEmptyOrNil ifTrue: [^ self].
	self form: (Form fromFileNamed: fileName).
