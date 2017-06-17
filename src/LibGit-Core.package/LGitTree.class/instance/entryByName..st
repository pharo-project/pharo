accessing
entryByName: aString 
	^ self entryByName: aString ifAbsent: [ 
		self error: ('Entry {1} is not present in tree {2}' format: {aString. self})].
