accessing
changeForClass: aClassName selector: aSelector 
	^ (isMeta not and: [ self selector = aSelector and: [ className = aClassName ] ])
		ifTrue: [ self ]
		ifFalse: [ nil ]