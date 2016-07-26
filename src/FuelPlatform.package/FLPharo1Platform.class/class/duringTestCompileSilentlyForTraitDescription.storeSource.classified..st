private-extensions-traitdescription
duringTestCompileSilentlyForTraitDescription: code storeSource: storeSource classified: aCategory
	<flExtensionOf: #TraitDescription selector: #duringTestCompileSilently:storeSource:classified:>
	^ FLPlatform current 
		useAuthor: 'TestsAuthor'
		during: [ [
			self 
				compile: code
				classified: (aCategory ifNil: [ '''' ])
				withStamp: nil 
				notifying: nil 
				logSource: storeSource ] fuelValueWithoutNotifications ]