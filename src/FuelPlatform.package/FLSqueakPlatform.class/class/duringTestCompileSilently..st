private-extensions-classdescription
duringTestCompileSilently: code
	<flExtensionOf: #ClassDescription>
	^ self 
		duringTestCompileSilently: code 
		storeSource: true