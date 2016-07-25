execute
ensureContactInfo

	^ self hasContactInfo 
		ifFalse: [ self defineContactInfoTemplate ]