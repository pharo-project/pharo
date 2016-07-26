public
condense
	job := [ 
		newChangesFile writeStreamDo: [ :aStream |
			stream := aStream.
			self basicCondense ]] asJob.
	
	job 
		title: 'Condensing Changes';
		max: Smalltalk classNames size + Smalltalk traitNames size;
		run.