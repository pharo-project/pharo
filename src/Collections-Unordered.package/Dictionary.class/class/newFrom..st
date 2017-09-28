as yet unclassified
newFrom: aDict 
	"Answer an instance of me containing the same associations as aDict.
	 Error if any key appears twice."
	| newDictionary |
	newDictionary := self new: aDict size.
	aDict associationsDo:
		[:x |
		(newDictionary includesKey: x key)
			ifTrue: [self error: 'Duplicate key: ', x key printString]
			ifFalse: [newDictionary add: x]].
	^ newDictionary

"	NewDictionary newFrom: {1->#a. 2->#b. 3->#c}
	{1->#a. 2->#b. 3->#c} as: NewDictionary
	NewDictionary newFrom: {1->#a. 2->#b. 1->#c}
	{1->#a. 2->#b. 1->#c} as: NewDictionary
"