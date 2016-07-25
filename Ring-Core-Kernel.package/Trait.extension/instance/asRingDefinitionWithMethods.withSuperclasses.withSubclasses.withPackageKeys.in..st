*Ring-Core-Kernel
asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice
	
	| rgClass rgMethod rgSuper sup |	
	rgClass := self asRingDefinition.
	aRGSlice loadClass: rgClass using: packageKeys.

	methodsBoolean ifTrue: [
		self methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			aRGSlice loadMethod: rgMethod inClass: rgClass using: packageKeys ].
		self theMetaClass methodsDo:[ :mth|  
			rgMethod := mth asActiveRingDefinition.
			aRGSlice loadMethod: rgMethod inClass: rgClass theMetaClass using: packageKeys ] ].

	supersBoolean ifTrue: [
		rgSuper := aRGSlice classNamed: rgClass superclassName.
		rgSuper isNil ifTrue: [
			sup := aRGSlice environment classNamed: rgClass superclassName.
			rgSuper := sup asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice ].
		rgClass superclass: rgSuper ].
	
	^rgClass