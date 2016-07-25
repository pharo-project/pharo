accessing
fieldValue
	^ self hasEntity 
		ifTrue: [ self entity contents ] 
		ifFalse: [ nil ] 