private-extensions-classdescription
duringTestCompileSilently: code storeSource: storeSource
	<flExtensionOf: #ClassDescription>
	^ self
		duringTestCompileSilently: code
		storeSource: storeSource
		classified: ''