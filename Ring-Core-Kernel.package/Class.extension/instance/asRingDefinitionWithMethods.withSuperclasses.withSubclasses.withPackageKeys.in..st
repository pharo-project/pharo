*Ring-Core-Kernel
asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: packageKeys in: aRGSlice
	| rgClass rgMethod rgSuper rgSub subs |
	rgClass := self asRingDefinition.
	aRGSlice loadClass: rgClass using: packageKeys.
	methodsBoolean
		ifTrue: [ 
			self
				methodsDo: [ :mth | 
					rgMethod := mth asActiveRingDefinition.
					aRGSlice loadMethod: rgMethod inClass: rgClass using: packageKeys ].
			self theMetaClass
				methodsDo: [ :mth | 
					rgMethod := mth asActiveRingDefinition.
					aRGSlice loadMethod: rgMethod inClass: rgClass theMetaClass using: packageKeys ] ].
	supersBoolean
		ifTrue: [ 
			self superclass
				ifNotNil: [ 
					rgSuper := aRGSlice classNamed: self superclass name.
					rgSuper
						ifNil: [ 
							rgSuper := self superclass
								asRingDefinitionWithMethods: methodsBoolean
								withSuperclasses: supersBoolean
								withSubclasses: subsBoolean
								withPackageKeys: packageKeys
								in: aRGSlice ].
					rgClass superclass: rgSuper ] ].
	subsBoolean
		ifTrue: [ 
			subs := self subclasses reject: [ :sub | sub isMeta ].
			rgClass name = #Trait
				ifTrue: [ subs := aRGSlice environment allTraits ].
			subs
				do: [ :each | 
					rgSub := aRGSlice classNamed: each name.
					rgSub
						ifNil: [ 
							rgSub := each
								asRingDefinitionWithMethods: methodsBoolean
								withSuperclasses: supersBoolean
								withSubclasses: subsBoolean
								withPackageKeys: packageKeys
								in: aRGSlice ].
					rgSub superclass: rgClass ] ].
	^ rgClass