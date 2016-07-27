private-extensions-methodcontext
tempNamed: aName
	<flExtensionOf: #MethodContext>
	| index |
	index := self tempNames indexOf: aName.
	^ self closure 
		ifNil: [ self tempAt: index ]
		ifNotNil: [ :closure | closure at: index ]