accessing
dependenciesFrom: aClass
	
	^ dependencies select: [ :reference | reference theClass = aClass ].