private 
toCategoryName: aString 
	^ aString = self packageName
		ifTrue: [ aString ]
		ifFalse: [ self packageName, '-', aString ]