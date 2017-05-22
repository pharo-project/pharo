comparing
= another
	^ self species = another species 
		and: [ self repository = another repository 
		and: [ self name = another name ]]
