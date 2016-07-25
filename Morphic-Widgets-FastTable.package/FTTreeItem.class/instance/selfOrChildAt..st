accessing
selfOrChildAt: anIndex
	^ anIndex = 1
		ifTrue: [ self ]
		ifFalse: [ self childAt: anIndex - 1 ]