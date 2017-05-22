accessing
entryByPath: aString 
	^ self entryByPath: aString ifAbsent: [ 
		self error: ('Entry {1} is not present in tree {2}' format: {aString. self})].
