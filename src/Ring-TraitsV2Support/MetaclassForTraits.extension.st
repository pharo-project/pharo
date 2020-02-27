Extension { #name : #MetaclassForTraits }

{ #category : #'*Ring-TraitsV2Support' }
MetaclassForTraits >> asRGDefinition [

	^ self theNonMetaClass asRGDefinition theMetaClass
]

{ #category : #'*Ring-TraitsV2Support' }
MetaclassForTraits >> asRingDefinitionIn: anRGEnvironment [

	^ RGMetaclassTrait named: self name parent: anRGEnvironment.

]

{ #category : #'*Ring-TraitsV2Support' }
MetaclassForTraits >> asRingMinimalDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGMetaclassTrait named: self name parent: anRGEnvironment]

]
