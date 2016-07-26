testing
isCombinableWith: anotherItem ifTrue: aBlockForTrue ifFalse: aBlockForFalse
	^ anotherItem class = self class
		ifFalse: [ 
			super 
				isCombinableWith: anotherItem 
				ifTrue: aBlockForTrue 
				ifFalse: aBlockForFalse ]
		ifTrue: [ 
			aBlockForTrue 
				value: (self copy
					entries: anotherItem entries, self entries;
					yourself) ]