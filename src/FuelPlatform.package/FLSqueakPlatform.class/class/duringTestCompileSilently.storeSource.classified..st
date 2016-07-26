private-extensions-classdescription
duringTestCompileSilently: code storeSource: storeSource classified: aCategory
	<flExtensionOf: #ClassDescription>
	^ FLPlatform current 
		useAuthor: 'TestsAuthor'
		during: [ [
			self 
				compile: code
				classified: (aCategory ifNil: [ '''' ])
				withStamp: nil 
				notifying: nil 
				logSource: storeSource ] fuelValueWithoutNotifications ]