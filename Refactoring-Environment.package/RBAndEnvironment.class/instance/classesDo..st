accessing
classesDo: aBlock 
	environment 
		classesDo: [:each | (self includesClass: each) ifTrue: [aBlock value: each]]