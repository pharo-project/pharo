private
copyDictionary: aDictionary 
	"We could send aDictionary the copy message, but that doesn't copy the associations."

	| newDictionary |
	newDictionary := Dictionary new: aDictionary size.
	aDictionary keysAndValuesDo: [ :key :value | newDictionary at: key put: value ].
	^ newDictionary