Trait {
	#name : #TRGReadOnlyTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
TRGReadOnlyTest >> testReadOnlyErrors [

	| selectors backend |
	
	selectors := #(#addAlias:to: #addBehavior:to: #addClassTag:to: #addClassVariable:to: #addDefinedBehavior:to: #addExclusion:to: #addExtensionMethod:to: #addGlobalVariable:to: #addInstanceVariable:to: #addLocalMethod:to: #addMethodTag:to: #addPackage:to: #addSharedPool:to: #addSlot:to: #addTransformation:to: #cleanAliasesFor: #cleanBehaviorsFor: #cleanClassTagsFor: #cleanClassVariablesFor: #cleanDefinedBehaviorsFor: #cleanExclusionsFor: #cleanExtensionMethodsFor: #cleanGlobalVariablesFor: #cleanInstanceVariablesFor: #cleanLocalMethodsFor: #cleanMethodTagsFor: #cleanPackagesFor: #cleanSharedPoolsFor: #cleanSlotsFor: #cleanTagsForClassesFor: #cleanTagsForMethodsFor: #cleanTransformationsFor: #createNewUnresolvedClass #createNewUnresolvedMetaclassFor: #createNewUnresolvedMetaclassMetaclassFor: #readOnlyError #removeAlias:from: #removeBehavior:from: #removeClassTag:from: #removeClassVariable:from: #removeDefinedBehavior:from: #removeExclusion:from: #removeExtensionMethod:from: #removeGlobalVariable:from: #removeInstanceVariable:from: #removeLocalMethod:from: #removeMethodTag:from: #removePackage:from: #removeSharedPool:from: #removeSlot:from: #removeTransformation:from: #setBaseClassFor:to: #setClassCommentAuthorFor:to: #setClassCommentContentFor:to: #setClassCommentTimeFor:to: #setClassTraitFor:to: #setCommentFor:to: #setExpressionFor:to: #setLayoutFor:to: #setMetaClassFor:to: #setMethodPackageFor:to: #setNameFor:to: #setPackageFor:to: #setProtocolFor:to: #setSourceCodeFor:to: #setSubjectFor:to: #setSuperclassFor:to: #setTraitCommentFor:to: #setTraitPackageFor:to: #tagClass:with: #tagMethod:with: #untagClass:from: #untagMethod:from: cleanProtocolsFor: removeProtocol:from: addProtocol:to:).
	
	backend := self backendClass new.
	
	selectors do: [:selector |
		self should: [backend perform: selector withEnoughArguments: #() ] raise: Error]

]
