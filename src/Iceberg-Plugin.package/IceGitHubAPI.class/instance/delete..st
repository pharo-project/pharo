private requesting
delete: aString 
	^ self withResponseValidationDo: [
		(self newRequestTo: aString)
			delete;
			response ]