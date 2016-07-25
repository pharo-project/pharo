copying
copyDictionary: aDictionary 
	| copy |
	copy := Dictionary new: aDictionary size.
	aDictionary keysAndValuesDo: [:key :value | copy at: key put: value].
	^copy