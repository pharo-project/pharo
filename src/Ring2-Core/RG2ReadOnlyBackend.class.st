Class {
	#name : #RG2ReadOnlyBackend,
	#superclass : #RG2EnvironmentBackend,
	#category : #'Ring2-Core-Environment'
}

{ #category : #'as yet unclassified' }
RG2ReadOnlyBackend class >> for: aRG2Environment [

	^ self new 
		environment: aRG2Environment;
		yourself.
]

{ #category : #'trait alias' }
RG2ReadOnlyBackend >> addAlias: aSymbol to: aRG2TraitAliasDefinition [

	self readOnlyError 
]

{ #category : #environment }
RG2ReadOnlyBackend >> addBehavior: aRG2BehaviorDefinition to: aRG2Environment [

	self readOnlyError.
]

{ #category : #package }
RG2ReadOnlyBackend >> addClassTag: aSymbol to: aRG2Package [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> addClassVariable: aRG2InstanceVariableDefinition to: aRG2Behavior [

	self readOnlyError.
]

{ #category : #package }
RG2ReadOnlyBackend >> addDefinedBehavior: aRG2BehaviorDefinition to: aRG2Package [

	self readOnlyError.
]

{ #category : #'trait exclusion' }
RG2ReadOnlyBackend >> addExclusion: aSymbol to: aRG2TraitExclusionDefinition [

	self readOnlyError 
]

{ #category : #package }
RG2ReadOnlyBackend >> addExtensionMethod: aRG2MethodDefinition to: aRG2Package [

	self readOnlyError.
]

{ #category : #environment }
RG2ReadOnlyBackend >> addGlobalVariable: aRG2GlobalVariableDefinition to: aRG2Environment [

	self readOnlyError.
]

{ #category : #'class description' }
RG2ReadOnlyBackend >> addInstanceVariable: aRG2InstanceVariableDefinition to: aRG2Behavior [

	self readOnlyError.
]

{ #category : #behavior }
RG2ReadOnlyBackend >> addLocalMethod: aRG2MethodDefinition to: aRG2Behavior [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> addMethodTag: aSymbol to: aRG2Behavior [

	self readOnlyError.
]

{ #category : #environment }
RG2ReadOnlyBackend >> addPackage: aRG2Package to: aRG2Environment [

	self readOnlyError.
]

{ #category : #behavior }
RG2ReadOnlyBackend >> addProtocol: aRG2ProtocolDefinition to: aRG2Behavior [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> addSharedPool: aRG2PoolVariableDefinition to: aRG2Behavior [

	self readOnlyError.
]

{ #category : #'pointer layout' }
RG2ReadOnlyBackend >> addSlot: aRG2SlotDefinition to: aRG2LayoutDefinition [

	self readOnlyError 
]

{ #category : #'trait composition' }
RG2ReadOnlyBackend >> addTransformation: aRG2TraitTransformationDefinition to: aRG2TraitCompositionDefinition [

	self readOnlyError 
]

{ #category : #'trait alias' }
RG2ReadOnlyBackend >> cleanAliasesFor: aRG2TraitAliasDefinition [

	self readOnlyError 
]

{ #category : #environment }
RG2ReadOnlyBackend >> cleanBehaviorsFor: aRG2Environment [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> cleanClassTagsFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> cleanClassVariablesFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #package }
RG2ReadOnlyBackend >> cleanDefinedBehaviorsFor: aRG2Package [

	self readOnlyError.
]

{ #category : #'trait exclusion' }
RG2ReadOnlyBackend >> cleanExclusionsFor: aRG2TraitExclusionDefinition [

	self readOnlyError 
]

{ #category : #package }
RG2ReadOnlyBackend >> cleanExtensionMethodsFor: aRG2Package [

	self readOnlyError.
]

{ #category : #environment }
RG2ReadOnlyBackend >> cleanGlobalVariablesFor: aRG2Environment [

	self readOnlyError.
]

{ #category : #'class description' }
RG2ReadOnlyBackend >> cleanInstanceVariablesFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #behavior }
RG2ReadOnlyBackend >> cleanLocalMethodsFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> cleanMethodTagsFor: aRG2MethodDefinition [

	self readOnlyError.
]

{ #category : #environment }
RG2ReadOnlyBackend >> cleanPackagesFor: aRG2Environment [

	self readOnlyError.
]

{ #category : #behavior }
RG2ReadOnlyBackend >> cleanProtocolsFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> cleanSharedPoolsFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #'pointer layout' }
RG2ReadOnlyBackend >> cleanSlotsFor: aRG2LayoutDefinition [

	self readOnlyError 
]

{ #category : #package }
RG2ReadOnlyBackend >> cleanTagsForClassesFor: aRG2Package [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> cleanTagsForMethodsFor: aRG2Behavior [

	self readOnlyError.
]

{ #category : #'trait composition' }
RG2ReadOnlyBackend >> cleanTransformationsFor: aRG2TraitCompositionDefinition [

	self readOnlyError 
]

{ #category : #'unresolved objects' }
RG2ReadOnlyBackend >> createNewUnresolvedClass [

	self readOnlyError 
"	| unresolvedValue |
	
	unresolvedValue := RG2UnresolvedValue new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue."

]

{ #category : #'unresolved objects' }
RG2ReadOnlyBackend >> createNewUnresolvedMetaclassFor: aRG2BehaviorDefinition [

	self readOnlyError 
	"^ RG2UnresolvedValue recursive"
]

{ #category : #'unresolved objects' }
RG2ReadOnlyBackend >> createNewUnresolvedMetaclassMetaclassFor: aRG2BehaviorDefinition [

	self readOnlyError 
	"| unresolvedValue |
	
	unresolvedValue := RG2UnresolvedValue new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue."
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyBackend >> readOnlyError [

	self error: 'read-only environment'
]

{ #category : #'trait alias' }
RG2ReadOnlyBackend >> removeAlias: aSymbol from: aRG2TraitAliasDefinition [

	self readOnlyError 
]

{ #category : #environment }
RG2ReadOnlyBackend >> removeBehavior: aRG2BehaviorDefinition from: aRG2Environment [

	self readOnlyError.
]

{ #category : #package }
RG2ReadOnlyBackend >> removeClassTag: aSymbol from: aRG2Package [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> removeClassVariable: aRG2InstanceVariableDefinition from: aRG2Behavior [

	self readOnlyError.
]

{ #category : #package }
RG2ReadOnlyBackend >> removeDefinedBehavior: aRG2BehaviorDefinition from: aRG2Package [

	self readOnlyError.
]

{ #category : #'trait exclusion' }
RG2ReadOnlyBackend >> removeExclusion: aSymbol from: aRG2TraitExclusionDefinition [

	self readOnlyError 
]

{ #category : #package }
RG2ReadOnlyBackend >> removeExtensionMethod: aRG2MethodDefinition from: aRG2Package [

	self readOnlyError.
]

{ #category : #environment }
RG2ReadOnlyBackend >> removeGlobalVariable: aRG2GlobalVariableDefinition from: aRG2Environment [

	self readOnlyError.
]

{ #category : #'class description' }
RG2ReadOnlyBackend >> removeInstanceVariable: aRG2InstanceVariableDefinition from: aRG2Behavior [

	self readOnlyError.
]

{ #category : #behavior }
RG2ReadOnlyBackend >> removeLocalMethod: aRG2MethodDefinition from: aRG2Behavior [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> removeMethodTag: aSymbol from: aRG2Behavior [

	self readOnlyError.
]

{ #category : #environment }
RG2ReadOnlyBackend >> removePackage: aRG2Package from: aRG2Environment [

	self readOnlyError.
]

{ #category : #behavior }
RG2ReadOnlyBackend >> removeProtocol: aRG2ProtocolDefinition from: aRG2Behavior [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> removeSharedPool: aRG2PoolVariableDefinition from: aRG2Behavior [

	self readOnlyError.
]

{ #category : #'pointer layout' }
RG2ReadOnlyBackend >> removeSlot: aRG2SlotDefinition from: aRG2LayoutDefinition [

	self readOnlyError 
]

{ #category : #'trait composition' }
RG2ReadOnlyBackend >> removeTransformation: aRG2TraitTransformationDefinition from: aRG2TraitCompositionDefinition [

	self readOnlyError 
]

{ #category : #metaclass }
RG2ReadOnlyBackend >> setBaseClassFor: aRG2Behavior to: aaRG2ClassDefinition [

	self readOnlyError.
]

{ #category : #'class comment' }
RG2ReadOnlyBackend >> setClassCommentAuthorFor: aRG2CommentDefinition to: aString [

	self readOnlyError.
]

{ #category : #'class comment' }
RG2ReadOnlyBackend >> setClassCommentContentFor: aRG2CommentDefinition to: anObject [

	self readOnlyError.
]

{ #category : #'class comment' }
RG2ReadOnlyBackend >> setClassCommentTimeFor: aRG2CommentDefinition to: aDateAndTime [

	self readOnlyError.
]

{ #category : #trait }
RG2ReadOnlyBackend >> setClassTraitFor: aRG2TraitDefinition to: aRG2MetatraitDefinition [

	self readOnlyError 
]

{ #category : #class }
RG2ReadOnlyBackend >> setCommentFor: aRG2Behavior to: aRG2CommentDefinition [

	self readOnlyError.
]

{ #category : #slot }
RG2ReadOnlyBackend >> setExpressionFor: aRG2UnknownSlotDefinition to: aString [

	self readOnlyError
]

{ #category : #behavior }
RG2ReadOnlyBackend >> setLayoutFor: aRG2BehaviorDefinition to: aRG2LayoutDefinition [

	self readOnlyError.
]

{ #category : #'class description' }
RG2ReadOnlyBackend >> setMetaClassFor: aRG2Behavior to: aRG2MetaclassDefinition [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> setMethodPackageFor: aRG2MethodDefinition to: aRG2PackageDefinition [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> setMethodStampFor: aRG2MethodDefinition to: anObject [

	self readOnlyError.
]

{ #category : #general }
RG2ReadOnlyBackend >> setNameFor: aRG2Definition to: aString [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> setPackageFor: aRG2Behavior to: aaRG2Package [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> setProtocolFor: aRG2MethodDefinition to: anObject [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> setSourceCodeFor: aRG2MethodDefinition to: anObject [

	self readOnlyError.
]

{ #category : #'trait transormation' }
RG2ReadOnlyBackend >> setSubjectFor: aRG2TraitCompositionDefinition to: aRG2TraitDefinition [

	self readOnlyError 
]

{ #category : #behavior }
RG2ReadOnlyBackend >> setSuperclassFor: aRG2Behavior to: anObject [

	self readOnlyError.
]

{ #category : #trait }
RG2ReadOnlyBackend >> setTraitCommentFor: aRG2Behavior to: aRG2CommentDefinition [

	self readOnlyError 
]

{ #category : #trait }
RG2ReadOnlyBackend >> setTraitPackageFor: aRG2Behavior to: aRG2Package [

	self readOnlyError 
]

{ #category : #class }
RG2ReadOnlyBackend >> tagClass: aRG2Behavior with: aSymbol [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> tagMethod: aRG2MethodDefinition with: aSymbol [

	self readOnlyError.
]

{ #category : #class }
RG2ReadOnlyBackend >> untagClass: aRG2Behavior from: aSymbol [

	self readOnlyError.
]

{ #category : #method }
RG2ReadOnlyBackend >> untagMethod: aRG2MethodDefinition from: aSymbol [

	self readOnlyError.
]
