accessing
manifestBuilderOfClass: aClass
	| key |
	key := aClass theNonMetaClass category.
	^ manifestClassCache
		at: key
		ifAbsentPut: [ 
			(self builderManifestClass hasManifestFor: aClass)
				ifTrue: [ self builderManifestClass of: aClass ]
				ifFalse: [ nil ] ]