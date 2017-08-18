accessing
classNamed: aClassName
	^ newClass name = aClassName
		ifTrue: [ newClass ]
		ifFalse: [ inner classNamed: aClassName ]