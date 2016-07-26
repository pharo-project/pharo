accessing
manifestBuilderOfPackage: aPackage
	| key |
	key := aPackage packageName.
	^ manifestClassCache
		at: key
		ifAbsentPut: [ 
			(self builderManifestClass hasPackageNamed: key)
				ifTrue: [ self builderManifestClass ofPackageNamed: key ]
				ifFalse: [ nil ] ]