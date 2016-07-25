private
priorVersionOfAMethod: aMethodReference
	| tempList |
	
	tempList := referencesList select:[:each | 
		(each className = aMethodReference className) & 
		(each name = aMethodReference name)].
	^ tempList detect: [:each | (self versionOfAMethod: each) = ((self versionOfAMethod: aMethodReference) -1)] ifNone: [aMethodReference].