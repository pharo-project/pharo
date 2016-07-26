printing
printOn: aStream

	aStream
		nextPutAll: 'aKMCategoryTarget(';
		nextPutAll: (category name ifNil: 'nil' ifNotNil: [ :n | n printString ]);
		nextPutAll: ')'.