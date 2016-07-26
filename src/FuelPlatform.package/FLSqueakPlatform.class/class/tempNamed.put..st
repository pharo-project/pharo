private-extensions-methodcontext
tempNamed: aName put: anObject
	<flExtensionOf: #MethodContext>
	^ self tempAt: (self tempNames indexOf: aName) put: anObject