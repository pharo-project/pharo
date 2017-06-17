private requesting
post: aString with: aBlock  
	^ self withResponseValidationDo: [
		(self newRequestTo: aString)
			in: aBlock;
			post;
			response ]