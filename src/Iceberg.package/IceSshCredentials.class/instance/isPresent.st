testing
isPresent
	^ self username notEmpty 
		and: [ self publicKey notEmpty 
		and: [ self privateKey notEmpty ] ]