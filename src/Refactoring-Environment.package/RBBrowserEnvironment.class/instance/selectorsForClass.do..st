accessing
selectorsForClass: aClass do: aBlock 
	aClass selectorsAndMethodsDo: 
			[:each :meth | 
			(self includesSelector: each in: aClass) ifTrue: [aBlock value: each]]