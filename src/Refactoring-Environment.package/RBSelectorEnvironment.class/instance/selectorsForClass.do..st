accessing
selectorsForClass: aClass do: aBlock 
	^(self privateSelectorsForClass: aClass) 
		do: [:each | (aClass includesSelector: each) ifTrue: [aBlock value: each]]