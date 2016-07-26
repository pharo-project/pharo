private
versionOfAMethod: aMethodReference
	| tempList |
	
	tempList := referencesList select: [:each | 
		(each className = aMethodReference className) & 
		(each name = aMethodReference name)].
	tempList := tempList sort: [:m1 :m2 | m1 timeStamp < m2 timeStamp].
	^ tempList identityIndexOf: aMethodReference ifAbsent: [0]