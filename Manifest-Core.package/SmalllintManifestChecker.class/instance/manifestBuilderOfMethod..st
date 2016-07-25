accessing
manifestBuilderOfMethod: aMethod
	| key cl |
	cl := aMethod methodClass.
	key := cl name , aMethod category asString.
	^ manifestClassCache
		at: key
		ifAbsentPut: [ 
			(self builderManifestClass hasManifestFor: aMethod)
				ifTrue: [ self builderManifestClass of: aMethod ]
				ifFalse: [ nil ] ]