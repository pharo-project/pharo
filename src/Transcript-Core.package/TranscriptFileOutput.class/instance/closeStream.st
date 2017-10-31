private
closeStream
	
	fileStream ifNil: [ ^self ].	

	fileStream flush; close.
	
	fileStream := nil.
