Class {
	#name : #RG2EnvironmentBackend,
	#superclass : #Object,
	#instVars : [
		'environment',
		'providedDefinitions'
	],
	#category : #'Ring2-Core-Environment'
}

{ #category : #'as yet unclassified' }
RG2EnvironmentBackend class >> for: aRG2Environment [

	^ self new 
		environment: aRG2Environment;
		yourself.
]

{ #category : #'trait alias' }
RG2EnvironmentBackend >> addAlias: aSymbol to: aRG2TraitAliasDefinition [

	^ aRG2TraitAliasDefinition pvtAddAlias: aSymbol
]

{ #category : #environment }
RG2EnvironmentBackend >> addBehavior: aRG2BehaviorDefinition to: aRG2Environment [

	^ aRG2Environment pvtAddBehavior: aRG2BehaviorDefinition
]

{ #category : #package }
RG2EnvironmentBackend >> addClassTag: aSymbol to: aRG2Package [

	^ aRG2Package pvtAddClassTag: aSymbol
]

{ #category : #class }
RG2EnvironmentBackend >> addClassVariable: aRG2InstanceVariableDefinition to: aRG2Behavior [

	^ aRG2Behavior pvtAddClassVariable: aRG2InstanceVariableDefinition
]

{ #category : #package }
RG2EnvironmentBackend >> addDefinedBehavior: aRG2BehaviorDefinition to: aRG2Package [

	^ aRG2Package pvtAddDefinedBehavior: aRG2BehaviorDefinition
]

{ #category : #'trait exclusion' }
RG2EnvironmentBackend >> addExclusion: aSymbol to: aRG2TraitExclusionDefinition [

	^ aRG2TraitExclusionDefinition pvtAddExclusion: aSymbol
]

{ #category : #package }
RG2EnvironmentBackend >> addExtensionMethod: aRG2MethodDefinition to: aRG2Package [

	^ aRG2Package pvtAddExtensionMethod: aRG2MethodDefinition
]

{ #category : #environment }
RG2EnvironmentBackend >> addGlobalVariable: aRG2GlobalVariableDefinition to: aRG2Environment [

	^ aRG2Environment pvtAddGlobalVariable: aRG2GlobalVariableDefinition
]

{ #category : #behavior }
RG2EnvironmentBackend >> addLocalMethod: aRG2MethodDefinition to: aRG2Behavior [

	^ aRG2Behavior pvtAddLocalMethod: aRG2MethodDefinition
]

{ #category : #class }
RG2EnvironmentBackend >> addMethodTag: aSymbol to: aRG2Behavior [

	^ aRG2Behavior pvtAddMethodTag: aSymbol
]

{ #category : #environment }
RG2EnvironmentBackend >> addPackage: aRG2Package to: aRG2Environment [

	^ aRG2Environment pvtAddPackage: aRG2Package
]

{ #category : #class }
RG2EnvironmentBackend >> addSharedPool: aRG2PoolVariableDefinition to: aRG2Behavior [

	^ aRG2Behavior pvtAddSharedPool: aRG2PoolVariableDefinition
]

{ #category : #'pointer layout' }
RG2EnvironmentBackend >> addSlot: aRG2SlotDefinition to: aRG2LayoutDefinition [

	^ aRG2LayoutDefinition pvtAddSlot: aRG2SlotDefinition
]

{ #category : #'trait composition' }
RG2EnvironmentBackend >> addTransformation: aRG2TraitTransformationDefinition to: aRG2TraitCompositionDefinition [

	^ aRG2TraitCompositionDefinition pvtAddTransformation: aRG2TraitTransformationDefinition
]

{ #category : #'trait alias' }
RG2EnvironmentBackend >> aliasesFor: aRG2TraitAliasDefinition do: aBlock [

	^ aRG2TraitAliasDefinition pvtAliasesDo: aBlock
]

{ #category : #method }
RG2EnvironmentBackend >> astFor: aRG2MethodDefinition [

	^  aRG2MethodDefinition astFromSource
]

{ #category : #method }
RG2EnvironmentBackend >> authorFor: aRG2MethodDefinition [

	^  aRG2MethodDefinition pvtAuthor
]

{ #category : #metaclass }
RG2EnvironmentBackend >> baseClassFor: aRG2Behavior [

	^  aRG2Behavior pvtBaseClass
]

{ #category : #environment }
RG2EnvironmentBackend >> behaviorsFor: aRG2Environment do: aBlock [

	^  aRG2Environment pvtBehaviorsDo: aBlock
]

{ #category : #'class comment' }
RG2EnvironmentBackend >> classCommentAuthorFor: aRG2CommentDefinition [

	^  aRG2CommentDefinition pvtAuthor
]

{ #category : #'class comment' }
RG2EnvironmentBackend >> classCommentContentFor: aRG2CommentDefinition [

	^ aRG2CommentDefinition pvtContent

]

{ #category : #'class comment' }
RG2EnvironmentBackend >> classCommentTimeFor: aRG2CommentDefinition [

	^  aRG2CommentDefinition pvtTime
]

{ #category : #trait }
RG2EnvironmentBackend >> classTraitFor: aRG2TraitDefinition [

	^  aRG2TraitDefinition pvtClassTrait
]

{ #category : #class }
RG2EnvironmentBackend >> classVariablesFor: aRG2Behavior do: aBlock [

	^ aRG2Behavior pvtClassVariablesDo: aBlock
]

{ #category : #'trait alias' }
RG2EnvironmentBackend >> cleanAliasesFor: aRG2TraitAliasDefinition [

	^ aRG2TraitAliasDefinition pvtCleanAliases
]

{ #category : #environment }
RG2EnvironmentBackend >> cleanBehaviorsFor: aRG2Environment [

	aRG2Environment pvtCleanBehaviors.
]

{ #category : #class }
RG2EnvironmentBackend >> cleanClassTagsFor: aRG2Behavior [

	aRG2Behavior pvtCleanTags.
]

{ #category : #class }
RG2EnvironmentBackend >> cleanClassVariablesFor: aRG2Behavior [

	^ aRG2Behavior pvtCleanClassVariables
]

{ #category : #package }
RG2EnvironmentBackend >> cleanDefinedBehaviorsFor: aRG2Package [

	aRG2Package pvtCleanDefinedBehaviors.
]

{ #category : #'trait exclusion' }
RG2EnvironmentBackend >> cleanExclusionsFor: aRG2TraitExclusionDefinition [

	^ aRG2TraitExclusionDefinition pvtCleanExclusions
]

{ #category : #package }
RG2EnvironmentBackend >> cleanExtensionMethodsFor: aRG2Package [

	aRG2Package pvtCleanExtensionMethods
]

{ #category : #environment }
RG2EnvironmentBackend >> cleanGlobalVariablesFor: aRG2Environment [

	^ aRG2Environment pvtCleanGlobalVariables
]

{ #category : #behavior }
RG2EnvironmentBackend >> cleanLocalMethodsFor: aRG2Behavior [

	^ aRG2Behavior pvtCleanLocalMethods
]

{ #category : #method }
RG2EnvironmentBackend >> cleanMethodTagsFor: aRG2MethodDefinition [

	aRG2MethodDefinition pvtCleanTags.
]

{ #category : #environment }
RG2EnvironmentBackend >> cleanPackagesFor: aRG2Environment [

	aRG2Environment pvtCleanPackages
]

{ #category : #class }
RG2EnvironmentBackend >> cleanSharedPoolsFor: aRG2Behavior [

	^ aRG2Behavior pvtCleanSharedPools
]

{ #category : #'pointer layout' }
RG2EnvironmentBackend >> cleanSlotsFor: aRG2LayoutDefinition [

	^ aRG2LayoutDefinition pvtCleanSlots
]

{ #category : #package }
RG2EnvironmentBackend >> cleanTagsForClassesFor: aRG2Package [

	aRG2Package pvtCleanTagsForClasses.
]

{ #category : #class }
RG2EnvironmentBackend >> cleanTagsForMethodsFor: aRG2Behavior [

	aRG2Behavior pvtCleanTagsForMethods.
]

{ #category : #'trait composition' }
RG2EnvironmentBackend >> cleanTransformationsFor: aRG2TraitCompositionDefinition [

	^ aRG2TraitCompositionDefinition pvtCleanTransformations
]

{ #category : #class }
RG2EnvironmentBackend >> commentFor: aRG2Behavior [

	^  aRG2Behavior pvtComment
]

{ #category : #'unresolved objects' }
RG2EnvironmentBackend >> createNewUnresolvedClass [

	| classStub metaclassStub packageStub |
	
	classStub := RG2ClassDefinition unresolvedWithParent: self environment.
	packageStub := RG2PackageDefinition unresolvedWithParent: self environment.
	classStub pvtPackage: packageStub.
	packageStub pvtAddDefinedBehavior: classStub.
	metaclassStub := self createNewUnresolvedMetaclassFor: classStub.

	classStub pvtSuperclass: classStub.
	classStub pvtMetaclass: metaclassStub.
	classStub pvtPackage: packageStub.
	packageStub pvtAddDefinedBehavior: classStub.
	self environment pvtAddBehavior: classStub.
	self environment pvtAddPackage: packageStub.

	^ classStub
	


]

{ #category : #'unresolved objects' }
RG2EnvironmentBackend >> createNewUnresolvedMetaclassFor: aRG2BehaviorDefinition [

	| metaclassStub superclassMetaclass |
	
	metaclassStub := RG2MetaclassDefinition unresolvedWithParent: self environment.

	superclassMetaclass := aRG2BehaviorDefinition pvtSuperclass pvtMetaclass.
	superclassMetaclass 
		ifNotNil: [ 
			metaclassStub pvtSuperclass: superclassMetaclass.
			metaclassStub pvtMetaclass: superclassMetaclass ]
		ifNil: [ 
			metaclassStub pvtSuperclass: metaclassStub.
			metaclassStub pvtMetaclass: metaclassStub ].
	self environment pvtAddBehavior: metaclassStub.

	^ metaclassStub	

]

{ #category : #'unresolved objects' }
RG2EnvironmentBackend >> createUnresolvedClassGroupFor: aRG2BehaviorDefinition [ 

	| classStub metaclassStub superclassStub superclassMetaclassStub packageStub1 packageStub2 |

	classStub := aRG2BehaviorDefinition isMeta
		ifFalse: [ aRG2BehaviorDefinition ]
		ifTrue: [ RG2ClassDefinition unresolvedWithParent: self environment ].
	classStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	classStub propertyNamed: #role put: #class.
		
	metaclassStub := aRG2BehaviorDefinition isMeta
		ifFalse: [ RG2MetaclassDefinition unresolvedWithParent: self environment ]
		ifTrue: [ aRG2BehaviorDefinition ].
	metaclassStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	metaclassStub propertyNamed: #role put: #metaclass.

	superclassStub := RG2ClassDefinition unresolvedWithParent: self environment.
	superclassStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	superclassStub propertyNamed: #role put: #superclass.

	superclassMetaclassStub := RG2MetaclassDefinition unresolvedWithParent: self environment.
	superclassMetaclassStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	superclassMetaclassStub propertyNamed: #role put: #superclassMetaclass.
	
	packageStub1 := RG2PackageDefinition unresolvedWithParent: self environment.
	classStub pvtPackage: packageStub1.
	self environment pvtAddPackage: packageStub1.
	packageStub1 pvtAddDefinedBehavior: classStub.
	packageStub1 propertyNamed: #creator put: aRG2BehaviorDefinition.
	packageStub1 propertyNamed: #role put: #package.
	
	packageStub2 := RG2PackageDefinition unresolvedWithParent: self environment.
	superclassStub pvtPackage: packageStub2.
	self environment pvtAddPackage: packageStub2.
	packageStub2 pvtAddDefinedBehavior: superclassStub.
	packageStub2 propertyNamed: #creator put: aRG2BehaviorDefinition.
	packageStub2 propertyNamed: #role put: #superclassPackage.

	classStub pvtSuperclass: superclassStub.
	classStub pvtMetaclass: metaclassStub.
	
	superclassStub pvtSuperclass: superclassStub.
	superclassStub pvtMetaclass: superclassMetaclassStub.
	
	metaclassStub pvtSuperclass: superclassMetaclassStub.
	metaclassStub pvtMetaclass: superclassMetaclassStub.
	metaclassStub pvtBaseClass: classStub.
	
	superclassMetaclassStub pvtSuperclass: superclassMetaclassStub.
	superclassMetaclassStub pvtMetaclass: superclassMetaclassStub.
	superclassMetaclassStub pvtBaseClass: superclassStub.
	
	self environment pvtAddBehavior: classStub.
	self environment pvtAddBehavior: metaclassStub. 	
	self environment pvtAddBehavior: superclassStub.
	self environment pvtAddBehavior: superclassMetaclassStub.
	

	^ 	aRG2BehaviorDefinition isMeta
		ifFalse: [ metaclassStub ]
		ifTrue: [ classStub ].

]

{ #category : #'unresolved objects' }
RG2EnvironmentBackend >> createUnresolvedTraitGroupFor: aRG2BehaviorDefinition [ 

	| traitStub classTraitStub traitMetaclassStub classTraitMetaclassStub packageStub1  |

	traitStub := aRG2BehaviorDefinition isMeta
		ifFalse: [ aRG2BehaviorDefinition ]
		ifTrue: [ RG2TraitDefinition unresolvedWithParent: self environment ].
	traitStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	traitStub propertyNamed: #role put: #trait.
		
	classTraitStub := aRG2BehaviorDefinition isMeta
		ifFalse: [ RG2MetaclassTraitDefinition unresolvedWithParent: self environment ]
		ifTrue: [ aRG2BehaviorDefinition ].
	classTraitStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	classTraitStub propertyNamed: #role put: #classTrait.

	"Trait"
	traitMetaclassStub := RG2ClassDefinition unresolvedWithParent: self environment.
	self createUnresolvedClassGroupFor: traitMetaclassStub.
	traitMetaclassStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	traitMetaclassStub propertyNamed: #role put: #traitMetaclass.
	
	"ClassTrait"
	classTraitMetaclassStub := RG2ClassDefinition unresolvedWithParent: self environment.
	self createUnresolvedClassGroupFor: classTraitMetaclassStub.
	classTraitMetaclassStub propertyNamed: #creator put: aRG2BehaviorDefinition.
	classTraitMetaclassStub propertyNamed: #role put: #classTraitMetaclass.

	packageStub1 := RG2PackageDefinition unresolvedWithParent: self environment.
	packageStub1 propertyNamed: #creator put: aRG2BehaviorDefinition.
	packageStub1 propertyNamed: #role put: #traitMetaclassPackage.

	traitStub pvtPackage: packageStub1.
	packageStub1 pvtAddDefinedBehavior: traitStub.
	traitStub pvtSuperclass: traitStub.
	traitStub pvtMetaclass: traitMetaclassStub.
	traitStub pvtClassTrait: classTraitStub.

	classTraitStub pvtSuperclass: classTraitStub.
	classTraitStub pvtMetaclass: classTraitMetaclassStub.
	classTraitStub pvtBaseTrait: traitStub.
	
	self environment pvtAddBehavior: traitStub.		
	self environment pvtAddBehavior: classTraitStub.	
	self environment pvtAddBehavior: traitMetaclassStub.
	self environment pvtAddBehavior: classTraitMetaclassStub.
	self environment pvtAddPackage: packageStub1.

	^ 	aRG2BehaviorDefinition isMeta
		ifFalse: [ traitStub ]
		ifTrue: [ classTraitStub ].

]

{ #category : #package }
RG2EnvironmentBackend >> definedBehaviorsFor: aRG2Package do: aBlock [

	^  aRG2Package pvtDefinedBehaviorsDo: aBlock
]

{ #category : #accesing }
RG2EnvironmentBackend >> definitionFor: anObject [ 

	^ self definitionFor: anObject ifAbsentRegister: [ anObject ensureRing2DefinitionIn: self environment ]

]

{ #category : #accesing }
RG2EnvironmentBackend >> definitionFor: anObject ifAbsentRegister: aDefinitionOrBlock [

	^ providedDefinitions at: anObject 
		ifPresent: [:definition | definition ]
		ifAbsentPut: [ aDefinitionOrBlock value ].
]

{ #category : #accesing }
RG2EnvironmentBackend >> environment [
		
	^ environment
]

{ #category : #accesing }
RG2EnvironmentBackend >> environment: aRG2Enironment [
		
	environment := aRG2Enironment
]

{ #category : #'trait exclusion' }
RG2EnvironmentBackend >> exclusionsFor: aRG2TraitExclusionDefinition do: aBlock [

	^ aRG2TraitExclusionDefinition pvtExclusionsDo: aBlock
]

{ #category : #slot }
RG2EnvironmentBackend >> expressionFor: aRG2UnknownSlotDefinition [

	^  aRG2UnknownSlotDefinition pvtExpression
]

{ #category : #package }
RG2EnvironmentBackend >> extensionMethodsFor: aRG2Package do: aBlock [

	^ aRG2Package pvtExtensionMethodsDo: aBlock
]

{ #category : #subbackends }
RG2EnvironmentBackend >> forBehavior [

	^ self 
]

{ #category : #subbackends }
RG2EnvironmentBackend >> forEnvironment [

	^ self 
]

{ #category : #subbackends }
RG2EnvironmentBackend >> forMethod [

	^ self 
]

{ #category : #subbackends }
RG2EnvironmentBackend >> forPackage [

	^ self 
]

{ #category : #environment }
RG2EnvironmentBackend >> globalVariablesFor: aRG2Environment do: aBlock [

	^ aRG2Environment pvtGlobalVariablesDo: aBlock
]

{ #category : #resolving }
RG2EnvironmentBackend >> hasFullyResolved: aRG2Definition [

	^ aRG2Definition pvtFullyResolved

]

{ #category : #resolving }
RG2EnvironmentBackend >> hasFullyUnresolved: aRG2Definition [		
		
	^ aRG2Definition pvtFullyUnresolved
]

{ #category : #method }
RG2EnvironmentBackend >> hasSourceCodeFor: aRG2MethodDefinition [ 

	^ aRG2MethodDefinition pvtSourceCode notNil
]

{ #category : #initialization }
RG2EnvironmentBackend >> initialize [

	super initialize.
	
	providedDefinitions := IdentityDictionary new.
]

{ #category : #behavior }
RG2EnvironmentBackend >> layoutFor: aRG2BehaviorDefinition [

	^  aRG2BehaviorDefinition pvtLayout
]

{ #category : #behavior }
RG2EnvironmentBackend >> localMethodsFor: aRG2Behavior do: aBlock [

	^ aRG2Behavior pvtLocalMethodsDo: aBlock
]

{ #category : #'as yet unclassified' }
RG2EnvironmentBackend >> metaclassFor: aRG2Behavior [

	^  aRG2Behavior pvtMetaclass
]

{ #category : #'metaclass trait' }
RG2EnvironmentBackend >> metaclassTraitBaseTraitFor: aRG2MetaclassTraitDefinition [

	^  aRG2MetaclassTraitDefinition pvtBaseTrait
]

{ #category : #method }
RG2EnvironmentBackend >> methodPackageFor: aRG2MethodDefinition [

	^  aRG2MethodDefinition pvtPackage
]

{ #category : #general }
RG2EnvironmentBackend >> nameFor: aRG2Definition [

	^ aRG2Definition pvtName
]

{ #category : #class }
RG2EnvironmentBackend >> packageFor: aRG2Behavior [

	^  aRG2Behavior pvtPackage
]

{ #category : #environment }
RG2EnvironmentBackend >> packagesFor: aRG2Environment do: aBlock [

	^  aRG2Environment pvtPackagesDo: aBlock
]

{ #category : #'trait alias' }
RG2EnvironmentBackend >> removeAlias: aSymbol from: aRG2TraitAliasDefinition [

	^ aRG2TraitAliasDefinition pvtRemoveAlias: aSymbol
]

{ #category : #environment }
RG2EnvironmentBackend >> removeBehavior: aRG2BehaviorDefinition from: aRG2Environment [

	aRG2Environment pvtRemoveBehavior: aRG2BehaviorDefinition.
	

]

{ #category : #package }
RG2EnvironmentBackend >> removeClassTag: aSymbol from: aRG2Package [

	^ aRG2Package pvtRemoveClassTag: aSymbol
]

{ #category : #class }
RG2EnvironmentBackend >> removeClassVariable: aRG2InstanceVariableDefinition from: aRG2Behavior [

	^ aRG2Behavior pvtRemoveClassVariable: aRG2InstanceVariableDefinition
]

{ #category : #package }
RG2EnvironmentBackend >> removeDefinedBehavior: aRG2BehaviorDefinition from: aRG2Package [

	^ aRG2Package pvtRemoveDefinedBehavior: aRG2BehaviorDefinition
]

{ #category : #'trait exclusion' }
RG2EnvironmentBackend >> removeExclusion: aSymbol from: aRG2TraitExclusionDefinition [

	^ aRG2TraitExclusionDefinition pvtRemoveExclusion: aSymbol
]

{ #category : #package }
RG2EnvironmentBackend >> removeExtensionMethod: aRG2MethodDefinition from: aRG2Package [

	^ aRG2Package pvtRemoveExtensionMethod: aRG2MethodDefinition
]

{ #category : #environment }
RG2EnvironmentBackend >> removeGlobalVariable: aRG2GlobalVariableDefinition from: aRG2Environment [

	^ aRG2Environment pvtRemoveGlobalVariable: aRG2GlobalVariableDefinition
]

{ #category : #behavior }
RG2EnvironmentBackend >> removeLocalMethod: aRG2MethodDefinition from: aRG2Behavior [

	^ aRG2Behavior pvtRemoveLocalMethod: aRG2MethodDefinition
]

{ #category : #class }
RG2EnvironmentBackend >> removeMethodTag: aSymbol from: aRG2Behavior [

	^ aRG2Behavior pvtRemoveMethodTag: aSymbol
]

{ #category : #environment }
RG2EnvironmentBackend >> removePackage: aRG2Package from: aRG2Environment [

	^ aRG2Environment pvtRemovePackage: aRG2Package
]

{ #category : #class }
RG2EnvironmentBackend >> removeSharedPool: aRG2PoolVariableDefinition from: aRG2Behavior [

	^ aRG2Behavior pvtRemoveSharedPool: aRG2PoolVariableDefinition
]

{ #category : #'pointer layout' }
RG2EnvironmentBackend >> removeSlot: aRG2SlotDefinition from: aRG2LayoutDefinition [

	^ aRG2LayoutDefinition pvtRemoveSlot: aRG2SlotDefinition
]

{ #category : #'trait composition' }
RG2EnvironmentBackend >> removeTransformation: aRG2TraitTransformationDefinition from: aRG2TraitCompositionDefinition [

	^ aRG2TraitCompositionDefinition pvtRemoveTransformation: aRG2TraitTransformationDefinition
]

{ #category : #resolving }
RG2EnvironmentBackend >> resolvedPropertiesFor: aRG2Definition [

	^ aRG2Definition pvtResolvedProperties
]

{ #category : #method }
RG2EnvironmentBackend >> setAuthorFor: aRG2MethodDefinition to: aDateAndTime [

	^  aRG2MethodDefinition pvtAuthor: aDateAndTime
]

{ #category : #metaclass }
RG2EnvironmentBackend >> setBaseClassFor: aRG2Behavior to: aaRG2ClassDefinition [

	^  aRG2Behavior pvtBaseClass: aaRG2ClassDefinition
]

{ #category : #'class comment' }
RG2EnvironmentBackend >> setClassCommentAuthorFor: aRG2CommentDefinition to: aString [

	^  aRG2CommentDefinition pvtAuthor: aString
]

{ #category : #'class comment' }
RG2EnvironmentBackend >> setClassCommentContentFor: aRG2CommentDefinition to: anObject [

	aRG2CommentDefinition pvtContent: anObject.

]

{ #category : #'class comment' }
RG2EnvironmentBackend >> setClassCommentTimeFor: aRG2CommentDefinition to: aDateAndTime [

	^  aRG2CommentDefinition pvtTime: aDateAndTime
]

{ #category : #trait }
RG2EnvironmentBackend >> setClassTraitFor: aRG2TraitDefinition to: aRG2MetatraitDefinition [

	^  aRG2TraitDefinition pvtClassTrait: aRG2MetatraitDefinition
]

{ #category : #class }
RG2EnvironmentBackend >> setCommentFor: aRG2Behavior to: aRG2CommentDefinition [

	^  aRG2Behavior pvtComment: aRG2CommentDefinition
]

{ #category : #slot }
RG2EnvironmentBackend >> setExpressionFor: aRG2UnknownSlotDefinition to: aString [

	^  aRG2UnknownSlotDefinition pvtExpression: aString
]

{ #category : #behavior }
RG2EnvironmentBackend >> setLayoutFor: aRG2BehaviorDefinition to: aRG2LayoutDefinition [

	^  aRG2BehaviorDefinition pvtLayout: aRG2LayoutDefinition
]

{ #category : #'as yet unclassified' }
RG2EnvironmentBackend >> setMetaclassFor: aRG2Behavior to: aRG2MetaclassDefinition [

	^  aRG2Behavior pvtMetaclass: aRG2MetaclassDefinition
]

{ #category : #'metaclass trait' }
RG2EnvironmentBackend >> setMetaclassTraitBaseTraitFor: aRG2MetaclassTraitDefinition to: aRG2TraitDefinition [

	^  aRG2MetaclassTraitDefinition pvtBaseTrait: aRG2TraitDefinition
]

{ #category : #method }
RG2EnvironmentBackend >> setMethodPackageFor: aRG2MethodDefinition to: aRG2PackageDefinition [

	^  aRG2MethodDefinition pvtPackage: aRG2PackageDefinition
]

{ #category : #general }
RG2EnvironmentBackend >> setNameFor: aRG2Definition to: aString [

	^ aRG2Definition pvtName: aString
]

{ #category : #class }
RG2EnvironmentBackend >> setPackageFor: aRG2Behavior to: aaRG2Package [

	^  aRG2Behavior pvtPackage: aaRG2Package
]

{ #category : #method }
RG2EnvironmentBackend >> setSourceCodeFor: aRG2MethodDefinition to: anObject [

	aRG2MethodDefinition pvtSourceCode: anObject
]

{ #category : #'trait transormation' }
RG2EnvironmentBackend >> setSubjectFor: aRG2TraitCompositionDefinition to: aRG2TraitDefinition [

	^  aRG2TraitCompositionDefinition pvtSubject: aRG2TraitDefinition
]

{ #category : #behavior }
RG2EnvironmentBackend >> setSuperclassFor: aRG2Behavior to: anObject [

	^  aRG2Behavior pvtSuperclass: anObject
]

{ #category : #method }
RG2EnvironmentBackend >> setTimeFor: aRG2MethodDefinition to: aDateAndTime [

	^  aRG2MethodDefinition pvtTime: aDateAndTime
]

{ #category : #trait }
RG2EnvironmentBackend >> setTraitCommentFor: aRG2Behavior to: aRG2CommentDefinition [

	^  aRG2Behavior pvtComment: aRG2CommentDefinition
]

{ #category : #behavior }
RG2EnvironmentBackend >> setTraitCompositionFor: aRG2BehaviorDefinition to: aRG2TraitCompositionDefinition [

	^  aRG2BehaviorDefinition pvtTraitComposition: aRG2TraitCompositionDefinition
]

{ #category : #trait }
RG2EnvironmentBackend >> setTraitPackageFor: aRG2Behavior to: aRG2Package [

	^  aRG2Behavior pvtPackage: aRG2Package
]

{ #category : #class }
RG2EnvironmentBackend >> sharedPoolsFor: aRG2Behavior do: aBlock [

	^ aRG2Behavior pvtSharedPoolsDo: aBlock
]

{ #category : #'pointer layout' }
RG2EnvironmentBackend >> slotsFor: aRG2LayoutDefinition do: aBlock [

	^ aRG2LayoutDefinition pvtSlotsDo: aBlock
]

{ #category : #method }
RG2EnvironmentBackend >> sourceCodeFor: aRG2MethodDefinition [ 

	^ aRG2MethodDefinition pvtSafeSourceCode

]

{ #category : #'trait transormation' }
RG2EnvironmentBackend >> subjectFor: aRG2TraitCompositionDefinition [

	^  aRG2TraitCompositionDefinition pvtSubject
]

{ #category : #behavior }
RG2EnvironmentBackend >> superclassFor: aRG2Behavior [

	^  aRG2Behavior pvtSuperclass
]

{ #category : #class }
RG2EnvironmentBackend >> tagClass: aRG2Behavior with: aSymbol [

	^ aRG2Behavior pvtTagWith: aSymbol
]

{ #category : #method }
RG2EnvironmentBackend >> tagMethod: aRG2MethodDefinition with: aSymbol [

	^ aRG2MethodDefinition pvtTagWith: aSymbol
]

{ #category : #class }
RG2EnvironmentBackend >> tagsForClass: aRG2Behavior do: aBlock [

	^  aRG2Behavior pvtTagsDo: aBlock
]

{ #category : #package }
RG2EnvironmentBackend >> tagsForClassesFor: aRG2Package do: aBlock [

	^  aRG2Package pvtTagsForClassesDo: aBlock
]

{ #category : #method }
RG2EnvironmentBackend >> tagsForMethod: aRG2MethodDefinition do: aBlock [

	^  aRG2MethodDefinition pvtTagsDo: aBlock
]

{ #category : #class }
RG2EnvironmentBackend >> tagsForMethodsFor: aRG2Behavior do: aBlock [

	^  aRG2Behavior pvtTagsForMethodsDo: aBlock
]

{ #category : #method }
RG2EnvironmentBackend >> timeFor: aRG2MethodDefinition [

	^  aRG2MethodDefinition pvtTime
]

{ #category : #trait }
RG2EnvironmentBackend >> traitCommentFor: aRG2Behavior [

	^  aRG2Behavior pvtComment
]

{ #category : #behavior }
RG2EnvironmentBackend >> traitCompositionFor: aRG2BehaviorDefinition [

	^  aRG2BehaviorDefinition pvtTraitComposition
]

{ #category : #trait }
RG2EnvironmentBackend >> traitPackageFor: aRG2Behavior [

	^  aRG2Behavior pvtPackage
]

{ #category : #'trait composition' }
RG2EnvironmentBackend >> transformationsFor: aRG2TraitCompositionDefinition do: aBlock [

	^ aRG2TraitCompositionDefinition pvtTransformationsDo: aBlock
]

{ #category : #resolving }
RG2EnvironmentBackend >> unresolvedPropertiesFor: aRG2Definition [

	^ aRG2Definition pvtUnresolvedProperties
]

{ #category : #class }
RG2EnvironmentBackend >> untagClass: aRG2Behavior from: aSymbol [

	^ aRG2Behavior pvtUntagFrom: aSymbol
]

{ #category : #method }
RG2EnvironmentBackend >> untagMethod: aRG2MethodDefinition from: aSymbol [

	^ aRG2MethodDefinition pvtUntagFrom: aSymbol
	

]
