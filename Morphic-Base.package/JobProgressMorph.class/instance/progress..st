API
progress: aNormalizedNumber
	bar progress = aNormalizedNumber
		ifFalse: [ 
			bar progress: aNormalizedNumber.
			self changed: #progressValue ].