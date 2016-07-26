accessing
selectorsForClass: aClass do: aBlock 
	environment selectorsForClass: aClass
		do: 
			[:each | 
			(andedEnvironment includesSelector: each in: aClass) 
				ifTrue: [aBlock value: each]]