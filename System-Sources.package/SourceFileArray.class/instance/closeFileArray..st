private
closeFileArray: anArray

	anArray 
		reject: [ :file | file isNil ] 
		thenDo: [ :file | file close ]
		