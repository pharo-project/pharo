special objects
specialSelectorIndexOrNil: selector
	| specialIndex |
	1 to: Smalltalk specialSelectorSize do:
		[:index | 
		(Smalltalk specialSelectorAt: index) == selector
			ifTrue: [specialIndex := index ]].	
	^ specialIndex