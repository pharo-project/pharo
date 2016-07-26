private-extensions-classdescription
duringTestCompileSilentlyForClassDescription: code storeSource: storeSource classified: aCategory
	<flExtensionOf: #ClassDescription selector: #duringTestCompileSilently:storeSource:classified:>
	^ FLPlatform current 
		useAuthor: 'TestsAuthor'
		during: [ [
			self 
				compile: code
				classified: (aCategory ifNil: [ '''' ])
				withStamp: nil 
				notifying: nil 
				logSource: storeSource ] fuelValueWithoutNotifications ]