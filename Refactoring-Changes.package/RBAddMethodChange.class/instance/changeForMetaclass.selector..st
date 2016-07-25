accessing
changeForMetaclass: aClassName selector: aSelector 
	^ (isMeta and: [ self selector = aSelector and: [ className = aClassName ] ])
		ifTrue: [ self ]
		ifFalse: [ nil ]