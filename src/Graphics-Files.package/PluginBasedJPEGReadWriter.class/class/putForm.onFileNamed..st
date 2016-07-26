image reading/writing
putForm: aForm onFileNamed: fileName 

	self 
	putForm: aForm 
	quality: -1 
	progressiveJPEG: false
	onFileNamed: fileName