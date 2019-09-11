Class {
	#name : #RGReadOnlyBackend,
	#superclass : #RGEnvironmentBackend,
	#category : #'Ring-Core-Environment'
}

{ #category : #'as yet unclassified' }
RGReadOnlyBackend class >> for: anRGEnvironment [

	^ self new 
		environment: anRGEnvironment;
		yourself.
]

{ #category : #'trait alias' }
RGReadOnlyBackend >> addAlias: aSymbol to: anRGTraitAlias [

	self readOnlyError 
]

{ #category : #environment }
RGReadOnlyBackend >> addBehavior: anRGBehavior to: anRGEnvironment [

	self readOnlyError.
]

{ #category : #package }
RGReadOnlyBackend >> addClassTag: aSymbol to: anRGPackageDefinition [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> addClassVariable: anRGInstanceVariableDefinition to: anRGBehavior [

	self readOnlyError.
]

{ #category : #package }
RGReadOnlyBackend >> addDefinedBehavior: anRGBehavior to: anRGPackage [

	self readOnlyError.
]

{ #category : #'trait exclusion' }
RGReadOnlyBackend >> addExclusion: aSymbol to: anRGTraitExclusion [

	self readOnlyError 
]

{ #category : #package }
RGReadOnlyBackend >> addExtensionMethod: anRGMethod to: anRGPackageDefinition [

	self readOnlyError.
]

{ #category : #environment }
RGReadOnlyBackend >> addGlobalVariable: anRGGlobalVariable to: anRGEnvironment [

	self readOnlyError.
]

{ #category : #'class description' }
RGReadOnlyBackend >> addInstanceVariable: anRGInstanceVariableDefinition to: anRGBehavior [

	self readOnlyError.
]

{ #category : #behavior }
RGReadOnlyBackend >> addLocalMethod: anRGMethod to: anRGBehavior [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> addMethodTag: aSymbol to: anRGBehavior [

	self readOnlyError.
]

{ #category : #environment }
RGReadOnlyBackend >> addPackage: anRGPackageDefinition to: anRGEnvironment [

	self readOnlyError.
]

{ #category : #behavior }
RGReadOnlyBackend >> addProtocol: anRGProtocolDefinition to: anRGBehavior [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> addSharedPool: anRGPoolVariable to: anRGBehavior [

	self readOnlyError.
]

{ #category : #'pointer layout' }
RGReadOnlyBackend >> addSlot: anRGSlot to: anRGLayout [

	self readOnlyError 
]

{ #category : #'trait composition' }
RGReadOnlyBackend >> addTransformation: anRGTraitTransformation to: anRGTraitComposition [

	self readOnlyError 
]

{ #category : #'trait alias' }
RGReadOnlyBackend >> cleanAliasesFor: anRGTraitAlias [

	self readOnlyError 
]

{ #category : #environment }
RGReadOnlyBackend >> cleanBehaviorsFor: anRGEnvironment [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> cleanClassTagsFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> cleanClassVariablesFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #package }
RGReadOnlyBackend >> cleanDefinedBehaviorsFor: anRGPackage [

	self readOnlyError.
]

{ #category : #'trait exclusion' }
RGReadOnlyBackend >> cleanExclusionsFor: anRGTraitExclusion [

	self readOnlyError 
]

{ #category : #package }
RGReadOnlyBackend >> cleanExtensionMethodsFor: anRGPackage [

	self readOnlyError.
]

{ #category : #environment }
RGReadOnlyBackend >> cleanGlobalVariablesFor: anRGEnvironment [

	self readOnlyError.
]

{ #category : #'class description' }
RGReadOnlyBackend >> cleanInstanceVariablesFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #behavior }
RGReadOnlyBackend >> cleanLocalMethodsFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> cleanMethodTagsFor: anRGMethod [

	self readOnlyError.
]

{ #category : #environment }
RGReadOnlyBackend >> cleanPackagesFor: anRGEnvironment [

	self readOnlyError.
]

{ #category : #behavior }
RGReadOnlyBackend >> cleanProtocolsFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> cleanSharedPoolsFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #'pointer layout' }
RGReadOnlyBackend >> cleanSlotsFor: anRGLayout [

	self readOnlyError 
]

{ #category : #package }
RGReadOnlyBackend >> cleanTagsForClassesFor: anRGPackage [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> cleanTagsForMethodsFor: anRGBehavior [

	self readOnlyError.
]

{ #category : #'trait composition' }
RGReadOnlyBackend >> cleanTransformationsFor: anRGTraitComposition [

	self readOnlyError 
]

{ #category : #'unresolved objects' }
RGReadOnlyBackend >> createNewUnresolvedClass [

	self readOnlyError 
"	| unresolvedValue |
	
	unresolvedValue := RGUnresolvedValue new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue."

]

{ #category : #'unresolved objects' }
RGReadOnlyBackend >> createNewUnresolvedMetaclassFor: anRGBehavior [

	self readOnlyError 
	"^ RGUnresolvedValue recursive"
]

{ #category : #'unresolved objects' }
RGReadOnlyBackend >> createNewUnresolvedMetaclassMetaclassFor: anRGBehavior [

	self readOnlyError 
	"| unresolvedValue |
	
	unresolvedValue := RGUnresolvedValue new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue."
]

{ #category : #'as yet unclassified' }
RGReadOnlyBackend >> readOnlyError [

	self error: 'read-only environment'
]

{ #category : #'trait alias' }
RGReadOnlyBackend >> removeAlias: aSymbol from: anRGTraitAlias [

	self readOnlyError 
]

{ #category : #environment }
RGReadOnlyBackend >> removeBehavior: anRGBehavior from: anRGEnvironment [

	self readOnlyError.
]

{ #category : #package }
RGReadOnlyBackend >> removeClassTag: aSymbol from: anRGPackageDefinition [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> removeClassVariable: anRGInstanceVariableDefinition from: anRGBehavior [

	self readOnlyError.
]

{ #category : #package }
RGReadOnlyBackend >> removeDefinedBehavior: anRGBehavior from: anRGPackage [

	self readOnlyError.
]

{ #category : #'trait exclusion' }
RGReadOnlyBackend >> removeExclusion: aSymbol from: anRGTraitExclusion [

	self readOnlyError 
]

{ #category : #package }
RGReadOnlyBackend >> removeExtensionMethod: anRGMethod from: anRGPackage [

	self readOnlyError.
]

{ #category : #environment }
RGReadOnlyBackend >> removeGlobalVariable: anRGGlobalVariable from: anRGEnvironment [

	self readOnlyError.
]

{ #category : #'class description' }
RGReadOnlyBackend >> removeInstanceVariable: anRGInstanceVariableDefinition from: anRGBehavior [

	self readOnlyError.
]

{ #category : #behavior }
RGReadOnlyBackend >> removeLocalMethod: anRGMethod from: anRGBehavior [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> removeMethodTag: aSymbol from: anRGBehavior [

	self readOnlyError.
]

{ #category : #environment }
RGReadOnlyBackend >> removePackage: anRGPackageDefinition from: anRGEnvironment [

	self readOnlyError.
]

{ #category : #behavior }
RGReadOnlyBackend >> removeProtocol: anRGProtocolDefinition from: anRGBehavior [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> removeSharedPool: anRGPoolVariable from: anRGBehavior [

	self readOnlyError.
]

{ #category : #'pointer layout' }
RGReadOnlyBackend >> removeSlot: anRGSlot from: anRGLayout [

	self readOnlyError 
]

{ #category : #'trait composition' }
RGReadOnlyBackend >> removeTransformation: anRGTraitTransformation from: anRGTraitComposition [

	self readOnlyError 
]

{ #category : #metaclass }
RGReadOnlyBackend >> setBaseClassFor: anRGBehavior to: anRGClass [

	self readOnlyError.
]

{ #category : #'class comment' }
RGReadOnlyBackend >> setClassCommentAuthorFor: anRGComment to: aString [

	self readOnlyError.
]

{ #category : #'class comment' }
RGReadOnlyBackend >> setClassCommentContentFor: anRGComment to: anObject [

	self readOnlyError.
]

{ #category : #'class comment' }
RGReadOnlyBackend >> setClassCommentTimeFor: anRGComment to: aDateAndTime [

	self readOnlyError.
]

{ #category : #trait }
RGReadOnlyBackend >> setClassTraitFor: anRGTrait to: anRGMetatraitDefinition [

	self readOnlyError 
]

{ #category : #class }
RGReadOnlyBackend >> setCommentFor: anRGBehavior to: anRGComment [

	self readOnlyError.
]

{ #category : #slot }
RGReadOnlyBackend >> setExpressionFor: anRGUnknownSlot to: aString [

	self readOnlyError
]

{ #category : #behavior }
RGReadOnlyBackend >> setLayoutFor: anRGBehavior to: anRGLayout [

	self readOnlyError.
]

{ #category : #'class description' }
RGReadOnlyBackend >> setMetaClassFor: anRGBehavior to: anRGMetaclass [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> setMethodPackageFor: anRGMethod to: anRGPackageDefinition [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> setMethodStampFor: anRGMethod to: anObject [

	self readOnlyError.
]

{ #category : #general }
RGReadOnlyBackend >> setNameFor: anRGObject to: aString [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> setPackageFor: anRGBehavior to: anRGPackageDefinition [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> setProtocolFor: anRGMethod to: anObject [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> setSourceCodeFor: anRGMethod to: anObject [

	self readOnlyError.
]

{ #category : #'trait transormation' }
RGReadOnlyBackend >> setSubjectFor: anRGTraitComposition to: anRGTrait [

	self readOnlyError 
]

{ #category : #behavior }
RGReadOnlyBackend >> setSuperclassFor: anRGBehavior to: anObject [

	self readOnlyError.
]

{ #category : #trait }
RGReadOnlyBackend >> setTraitCommentFor: anRGBehavior to: anRGComment [

	self readOnlyError 
]

{ #category : #trait }
RGReadOnlyBackend >> setTraitPackageFor: anRGBehavior to: anRGPackageDefinition [

	self readOnlyError 
]

{ #category : #class }
RGReadOnlyBackend >> tagClass: anRGBehavior with: aSymbol [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> tagMethod: anRGMethod with: aSymbol [

	self readOnlyError.
]

{ #category : #class }
RGReadOnlyBackend >> untagClass: anRGBehavior from: aSymbol [

	self readOnlyError.
]

{ #category : #method }
RGReadOnlyBackend >> untagMethod: anRGMethod from: aSymbol [

	self readOnlyError.
]
