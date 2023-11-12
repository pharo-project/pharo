Extension { #name : 'MetaclassForTraits' }

{ #category : '*Ring-TraitsSupport' }
MetaclassForTraits >> asRGDefinition [

	^ self instanceSide asRGDefinition classSide
]

{ #category : '*Ring-TraitsSupport' }
MetaclassForTraits >> asRingDefinitionIn: anRGEnvironment [

	^ RGMetaclassTrait named: self name parent: anRGEnvironment
]

{ #category : '*Ring-TraitsSupport' }
MetaclassForTraits >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGMetaclassTrait named: self name parent: anRGEnvironment]
]
