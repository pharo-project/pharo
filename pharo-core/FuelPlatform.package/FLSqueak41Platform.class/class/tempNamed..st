private-extensions-methodcontext
tempNamed: aName
	<flExtensionOf: #MethodContext>
	| index |
	index := self tempNames indexOf: aName.
	^ self closure 
		ifNil: [ self tempAt: index ]
		ifNotNilDo: [ :closure | closure at: index ]