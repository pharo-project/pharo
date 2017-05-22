private
errorToString: aDictionary 
	(aDictionary at: #code) = 'custom' ifTrue: [ ^ self customError: aDictionary ].
	(aDictionary at: #code) = 'invalid' ifTrue: [ ^ self invalidError: aDictionary ].
	
	^ self unknownError: aDictionary	