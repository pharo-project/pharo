testing
isCombinableWith: anotherItem ifTrue: aBlockForTrue ifFalse: aBlockForFalse
	^ anotherItem class = self class
		ifFalse: [ aBlockForFalse value ]
		 ifTrue: [ aBlockForTrue value: 
			(anotherItem copy
				filteredEntriesCount: (anotherItem filteredEntriesCount + filteredEntriesCount);
				yourself) ]