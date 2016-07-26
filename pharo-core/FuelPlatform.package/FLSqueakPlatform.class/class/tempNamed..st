private-extensions-methodcontext
tempNamed: aName
	<flExtensionOf: #MethodContext>
	^ self tempAt: (self tempNames indexOf: aName)