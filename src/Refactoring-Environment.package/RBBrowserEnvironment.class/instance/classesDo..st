accessing-classes
classesDo: aBlock 
	self systemDictionaryClassesDo: [ :each |
		(self includesClass: each)
			ifTrue: [aBlock value: each ] ]