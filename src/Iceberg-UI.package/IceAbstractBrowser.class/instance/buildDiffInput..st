building
buildDiffInput: change
	^ { 
		change myVersion ifNil: ''. 
		change theirVersion ifNil: ''
	}
