adding
declare: aKey from: aDictionary
	(self includesKey: aKey)
		ifTrue: [^ self].

	(aDictionary includesKey: aKey)
		ifTrue: [
			self add: (aDictionary associationAt: aKey).
			aDictionary removeKey: aKey]
		ifFalse: [self add: aKey -> nil]