accessing
previousEntityTo: anObject
	| entities index |
	entities := self entities.
	index := entities indexOf: anObject.
	^ index < 2 
		ifTrue: [ nil ]
		ifFalse: [ entities at: index - 1 ]