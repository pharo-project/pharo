Class {
	#name : #RGEnvironmentBackend,
	#superclass : #Object,
	#instVars : [
		'environment',
		'providedDefinitions'
	],
	#category : #'Ring-Core-Environment'
}

{ #category : #'as yet unclassified' }
RGEnvironmentBackend class >> for: anRGEnvironment [

	^ self new 
		environment: anRGEnvironment;
		yourself.
]

{ #category : #'trait alias' }
RGEnvironmentBackend >> addAlias: aSymbol to: anRGTraitAlias [

	^ anRGTraitAlias pvtAddAlias: aSymbol
]

{ #category : #environment }
RGEnvironmentBackend >> addBehavior: anRGBehavior to: anRGEnvironment [

	^ anRGEnvironment pvtAddBehavior: anRGBehavior
]

{ #category : #package }
RGEnvironmentBackend >> addClassTag: aSymbol to: anRGPackage [

	^ anRGPackage pvtAddClassTag: aSymbol
]

{ #category : #class }
RGEnvironmentBackend >> addClassVariable: anRGInstanceVariableDefinition to: anRGBehavior [

	^ anRGBehavior behaviorStrategy pvtAddClassVariable: anRGInstanceVariableDefinition
]

{ #category : #package }
RGEnvironmentBackend >> addDefinedBehavior: anRGBehavior to: anRGPackage [

	^ anRGPackage pvtAddDefinedBehavior: anRGBehavior
]

{ #category : #'trait exclusion' }
RGEnvironmentBackend >> addExclusion: aSymbol to: anRGTraitExclusion [

	^ anRGTraitExclusion pvtAddExclusion: aSymbol
]

{ #category : #package }
RGEnvironmentBackend >> addExtensionMethod: anRGMethod to: anRGPackage [

	^ anRGPackage pvtAddExtensionMethod: anRGMethod
]

{ #category : #environment }
RGEnvironmentBackend >> addGlobalVariable: anRGGlobalVariable to: anRGEnvironment [

	^ anRGEnvironment pvtAddGlobalVariable: anRGGlobalVariable
]

{ #category : #behavior }
RGEnvironmentBackend >> addLocalMethod: anRGMethod to: anRGBehavior [

	^ anRGBehavior pvtAddLocalMethod: anRGMethod
]

{ #category : #class }
RGEnvironmentBackend >> addMethodTag: aSymbol to: anRGBehavior [

	^ anRGBehavior pvtAddMethodTag: aSymbol
]

{ #category : #environment }
RGEnvironmentBackend >> addPackage: anRGPackage to: anRGEnvironment [

	^ anRGEnvironment pvtAddPackage: anRGPackage
]

{ #category : #class }
RGEnvironmentBackend >> addSharedPool: anRGPoolVariable to: anRGBehavior [

	^ anRGBehavior behaviorStrategy pvtAddSharedPool: anRGPoolVariable
]

{ #category : #'pointer layout' }
RGEnvironmentBackend >> addSlot: anRGSlot to: anRGLayout [

	^ anRGLayout pvtAddSlot: anRGSlot
]

{ #category : #'trait composition' }
RGEnvironmentBackend >> addTransformation: anRGTraitTransformation to: anRGTraitComposition [

	^ anRGTraitComposition pvtAddTransformation: anRGTraitTransformation
]

{ #category : #'trait alias' }
RGEnvironmentBackend >> aliasesFor: anRGTraitAlias do: aBlock [

	^ anRGTraitAlias pvtAliasesDo: aBlock
]

{ #category : #method }
RGEnvironmentBackend >> astFor: anRGMethod [

	^  anRGMethod astFromSource
]

{ #category : #method }
RGEnvironmentBackend >> authorFor: anRGMethod [

	^  anRGMethod pvtAuthor
]

{ #category : #metaclass }
RGEnvironmentBackend >> baseClassFor: anRGBehavior [

	^  anRGBehavior behaviorStrategy pvtBaseClass
]

{ #category : #environment }
RGEnvironmentBackend >> behaviorsFor: anRGEnvironment do: aBlock [

	^  anRGEnvironment pvtBehaviorsDo: aBlock
]

{ #category : #'class comment' }
RGEnvironmentBackend >> classCommentAuthorFor: anRGComment [

	^  anRGComment pvtAuthor
]

{ #category : #'class comment' }
RGEnvironmentBackend >> classCommentContentFor: anRGComment [

	^ anRGComment pvtContent

]

{ #category : #'class comment' }
RGEnvironmentBackend >> classCommentTimeFor: anRGComment [

	^  anRGComment pvtTime
]

{ #category : #trait }
RGEnvironmentBackend >> classTraitFor: anRGTrait [

	^  anRGTrait behaviorStrategy pvtClassTrait
]

{ #category : #class }
RGEnvironmentBackend >> classVariablesFor: anRGBehavior do: aBlock [

	^ anRGBehavior behaviorStrategy pvtClassVariablesDo: aBlock
]

{ #category : #'trait alias' }
RGEnvironmentBackend >> cleanAliasesFor: anRGTraitAlias [

	^ anRGTraitAlias pvtCleanAliases
]

{ #category : #environment }
RGEnvironmentBackend >> cleanBehaviorsFor: anRGEnvironment [

	anRGEnvironment pvtCleanBehaviors.
]

{ #category : #class }
RGEnvironmentBackend >> cleanClassTagsFor: anRGBehavior [

	anRGBehavior pvtCleanTags.
]

{ #category : #class }
RGEnvironmentBackend >> cleanClassVariablesFor: anRGBehavior [

	^ anRGBehavior behaviorStrategy pvtCleanClassVariables
]

{ #category : #package }
RGEnvironmentBackend >> cleanDefinedBehaviorsFor: anRGPackage [

	anRGPackage pvtCleanDefinedBehaviors.
]

{ #category : #'trait exclusion' }
RGEnvironmentBackend >> cleanExclusionsFor: anRGTraitExclusion [

	^ anRGTraitExclusion pvtCleanExclusions
]

{ #category : #package }
RGEnvironmentBackend >> cleanExtensionMethodsFor: anRGPackage [

	anRGPackage pvtCleanExtensionMethods
]

{ #category : #environment }
RGEnvironmentBackend >> cleanGlobalVariablesFor: anRGEnvironment [

	^ anRGEnvironment pvtCleanGlobalVariables
]

{ #category : #behavior }
RGEnvironmentBackend >> cleanLocalMethodsFor: anRGBehavior [

	^ anRGBehavior pvtCleanLocalMethods
]

{ #category : #method }
RGEnvironmentBackend >> cleanMethodTagsFor: anRGMethod [

	anRGMethod pvtCleanTags.
]

{ #category : #environment }
RGEnvironmentBackend >> cleanPackagesFor: anRGEnvironment [

	anRGEnvironment pvtCleanPackages
]

{ #category : #class }
RGEnvironmentBackend >> cleanSharedPoolsFor: anRGBehavior [

	^ anRGBehavior behaviorStrategy pvtCleanSharedPools
]

{ #category : #'pointer layout' }
RGEnvironmentBackend >> cleanSlotsFor: anRGLayout [

	^ anRGLayout pvtCleanSlots
]

{ #category : #package }
RGEnvironmentBackend >> cleanTagsForClassesFor: anRGPackage [

	anRGPackage pvtCleanTagsForClasses.
]

{ #category : #class }
RGEnvironmentBackend >> cleanTagsForMethodsFor: anRGBehavior [

	anRGBehavior pvtCleanTagsForMethods.
]

{ #category : #'trait composition' }
RGEnvironmentBackend >> cleanTransformationsFor: anRGTraitComposition [

	^ anRGTraitComposition pvtCleanTransformations
]

{ #category : #class }
RGEnvironmentBackend >> commentFor: anRGBehavior [

	^  anRGBehavior behaviorStrategy pvtComment
]

{ #category : #'unresolved objects' }
RGEnvironmentBackend >> createNewUnresolvedClass [

	| classStub metaclassStub packageStub |
	
	classStub := RGClass unresolvedWithParent: self environment.
	packageStub := RGPackage unresolvedWithParent: self environment.
	classStub behaviorStrategy pvtPackage: packageStub.
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
RGEnvironmentBackend >> createNewUnresolvedMetaclassFor: anRGBehavior [

	| metaclassStub superclassMetaclass |
	
	metaclassStub := RGMetaclass unresolvedWithParent: self environment.

	superclassMetaclass := anRGBehavior pvtSuperclass pvtMetaclass.
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
RGEnvironmentBackend >> createUnresolvedClassGroupFor: anRGBehavior [ 

	| classStub metaclassStub superclassStub superclassMetaclassStub packageStub1 packageStub2 |

	classStub := anRGBehavior isMeta
		ifFalse: [ anRGBehavior ]
		ifTrue: [ RGClass unresolvedWithParent: self environment ].
	classStub propertyNamed: #creator put: anRGBehavior.
	classStub propertyNamed: #role put: #class.
		
	metaclassStub := anRGBehavior isMeta
		ifFalse: [ RGMetaclass unresolvedWithParent: self environment ]
		ifTrue: [ anRGBehavior ].
	metaclassStub propertyNamed: #creator put: anRGBehavior.
	metaclassStub propertyNamed: #role put: #metaclass.

	superclassStub := RGClass unresolvedWithParent: self environment.
	superclassStub propertyNamed: #creator put: anRGBehavior.
	superclassStub propertyNamed: #role put: #superclass.

	superclassMetaclassStub := RGMetaclass unresolvedWithParent: self environment.
	superclassMetaclassStub propertyNamed: #creator put: anRGBehavior.
	superclassMetaclassStub propertyNamed: #role put: #superclassMetaclass.
	
	packageStub1 := RGPackage unresolvedWithParent: self environment.
	classStub behaviorStrategy pvtPackage: packageStub1.
	self environment pvtAddPackage: packageStub1.
	packageStub1 pvtAddDefinedBehavior: classStub.
	packageStub1 propertyNamed: #creator put: anRGBehavior.
	packageStub1 propertyNamed: #role put: #package.
	
	packageStub2 := RGPackage unresolvedWithParent: self environment.
	superclassStub behaviorStrategy pvtPackage: packageStub2.
	self environment pvtAddPackage: packageStub2.
	packageStub2 pvtAddDefinedBehavior: superclassStub.
	packageStub2 propertyNamed: #creator put: anRGBehavior.
	packageStub2 propertyNamed: #role put: #superclassPackage.

	classStub pvtSuperclass: superclassStub.
	classStub pvtMetaclass: metaclassStub.
	
	superclassStub pvtSuperclass: superclassStub.
	superclassStub pvtMetaclass: superclassMetaclassStub.
	
	metaclassStub pvtSuperclass: superclassMetaclassStub.
	metaclassStub pvtMetaclass: superclassMetaclassStub.
	metaclassStub behaviorStrategy pvtBaseClass: classStub.
	
	superclassMetaclassStub pvtSuperclass: superclassMetaclassStub.
	superclassMetaclassStub pvtMetaclass: superclassMetaclassStub.
	superclassMetaclassStub behaviorStrategy pvtBaseClass: superclassStub.
	
	self environment pvtAddBehavior: classStub.
	self environment pvtAddBehavior: metaclassStub. 	
	self environment pvtAddBehavior: superclassStub.
	self environment pvtAddBehavior: superclassMetaclassStub.
	

	^ 	anRGBehavior isMeta
		ifFalse: [ metaclassStub ]
		ifTrue: [ classStub ].

]

{ #category : #'unresolved objects' }
RGEnvironmentBackend >> createUnresolvedTraitGroupFor: anRGBehavior [ 

	| traitStub classTraitStub traitMetaclassStub classTraitMetaclassStub packageStub1  |

	traitStub := anRGBehavior isMeta
		ifFalse: [ anRGBehavior ]
		ifTrue: [ RGTrait unresolvedWithParent: self environment ].
	traitStub propertyNamed: #creator put: anRGBehavior.
	traitStub propertyNamed: #role put: #trait.
		
	classTraitStub := anRGBehavior isMeta
		ifFalse: [ RGMetaclassTrait unresolvedWithParent: self environment ]
		ifTrue: [ anRGBehavior ].
	classTraitStub propertyNamed: #creator put: anRGBehavior.
	classTraitStub propertyNamed: #role put: #classTrait.

	"Trait"
	traitMetaclassStub := RGClass unresolvedWithParent: self environment.
	self createUnresolvedClassGroupFor: traitMetaclassStub.
	traitMetaclassStub propertyNamed: #creator put: anRGBehavior.
	traitMetaclassStub propertyNamed: #role put: #traitMetaclass.
	
	"ClassTrait"
	classTraitMetaclassStub := RGClass unresolvedWithParent: self environment.
	self createUnresolvedClassGroupFor: classTraitMetaclassStub.
	classTraitMetaclassStub propertyNamed: #creator put: anRGBehavior.
	classTraitMetaclassStub propertyNamed: #role put: #classTraitMetaclass.

	packageStub1 := RGPackage unresolvedWithParent: self environment.
	packageStub1 propertyNamed: #creator put: anRGBehavior.
	packageStub1 propertyNamed: #role put: #traitMetaclassPackage.

	traitStub behaviorStrategy pvtPackage: packageStub1.
	packageStub1 pvtAddDefinedBehavior: traitStub.
	traitStub pvtSuperclass: traitStub.
	traitStub pvtMetaclass: traitMetaclassStub.
	traitStub behaviorStrategy pvtClassTrait: classTraitStub.

	classTraitStub pvtSuperclass: classTraitStub.
	classTraitStub pvtMetaclass: classTraitMetaclassStub.
	classTraitStub behaviorStrategy pvtBaseTrait: traitStub.
	
	self environment pvtAddBehavior: traitStub.		
	self environment pvtAddBehavior: classTraitStub.	
	self environment pvtAddBehavior: traitMetaclassStub.
	self environment pvtAddBehavior: classTraitMetaclassStub.
	self environment pvtAddPackage: packageStub1.

	^ 	anRGBehavior isMeta
		ifFalse: [ traitStub ]
		ifTrue: [ classTraitStub ].

]

{ #category : #package }
RGEnvironmentBackend >> definedBehaviorsFor: anRGPackage do: aBlock [

	^  anRGPackage pvtDefinedBehaviorsDo: aBlock
]

{ #category : #accesing }
RGEnvironmentBackend >> definitionFor: anObject [ 

	^ self definitionFor: anObject ifAbsentRegister: [ anObject ensureRingDefinitionIn: self environment ]

]

{ #category : #accesing }
RGEnvironmentBackend >> definitionFor: anObject ifAbsentRegister: aDefinitionOrBlock [

	^ providedDefinitions at: anObject 
		ifPresent: [:definition | definition ]
		ifAbsentPut: [ aDefinitionOrBlock value ].
]

{ #category : #accesing }
RGEnvironmentBackend >> environment [
		
	^ environment
]

{ #category : #accesing }
RGEnvironmentBackend >> environment: anRGEnironment [
		
	environment := anRGEnironment
]

{ #category : #'trait exclusion' }
RGEnvironmentBackend >> exclusionsFor: anRGTraitExclusion do: aBlock [

	^ anRGTraitExclusion pvtExclusionsDo: aBlock
]

{ #category : #slot }
RGEnvironmentBackend >> expressionFor: anRGUnknownSlot [

	^  anRGUnknownSlot pvtExpression
]

{ #category : #package }
RGEnvironmentBackend >> extensionMethodsFor: anRGPackage do: aBlock [

	^ anRGPackage pvtExtensionMethodsDo: aBlock
]

{ #category : #subbackends }
RGEnvironmentBackend >> forBehavior [

	^ self 
]

{ #category : #subbackends }
RGEnvironmentBackend >> forEnvironment [

	^ self 
]

{ #category : #subbackends }
RGEnvironmentBackend >> forMethod [

	^ self 
]

{ #category : #subbackends }
RGEnvironmentBackend >> forPackage [

	^ self 
]

{ #category : #environment }
RGEnvironmentBackend >> globalVariablesFor: anRGEnvironment do: aBlock [

	^ anRGEnvironment pvtGlobalVariablesDo: aBlock
]

{ #category : #resolving }
RGEnvironmentBackend >> hasFullyResolved: anRGObject [

	^ anRGObject pvtFullyResolved

]

{ #category : #resolving }
RGEnvironmentBackend >> hasFullyUnresolved: anRGObject [		
		
	^ anRGObject pvtFullyUnresolved
]

{ #category : #method }
RGEnvironmentBackend >> hasSourceCodeFor: anRGMethod [ 

	^ anRGMethod pvtSourceCode notNil
]

{ #category : #initialization }
RGEnvironmentBackend >> initialize [

	super initialize.
	
	providedDefinitions := IdentityDictionary new.
]

{ #category : #behavior }
RGEnvironmentBackend >> layoutFor: anRGBehavior [

	^  anRGBehavior behaviorStrategy pvtLayout
]

{ #category : #behavior }
RGEnvironmentBackend >> localMethodsFor: anRGBehavior do: aBlock [

	^ anRGBehavior pvtLocalMethodsDo: aBlock
]

{ #category : #'as yet unclassified' }
RGEnvironmentBackend >> metaclassFor: anRGBehavior [

	^  anRGBehavior pvtMetaclass
]

{ #category : #'metaclass trait' }
RGEnvironmentBackend >> metaclassTraitBaseTraitFor: anRGMetaclassTrait [

	^  anRGMetaclassTrait behaviorStrategy pvtBaseTrait
]

{ #category : #method }
RGEnvironmentBackend >> methodPackageFor: anRGMethod [

	^  anRGMethod pvtPackage
]

{ #category : #general }
RGEnvironmentBackend >> nameFor: anRGObject [

	^ anRGObject pvtName
]

{ #category : #class }
RGEnvironmentBackend >> packageFor: anRGBehavior [

	^  anRGBehavior behaviorStrategy pvtPackage
]

{ #category : #environment }
RGEnvironmentBackend >> packagesFor: anRGEnvironment do: aBlock [

	^  anRGEnvironment pvtPackagesDo: aBlock
]

{ #category : #'trait alias' }
RGEnvironmentBackend >> removeAlias: aSymbol from: anRGTraitAlias [

	^ anRGTraitAlias pvtRemoveAlias: aSymbol
]

{ #category : #environment }
RGEnvironmentBackend >> removeBehavior: anRGBehavior from: anRGEnvironment [

	anRGEnvironment pvtRemoveBehavior: anRGBehavior.
	

]

{ #category : #package }
RGEnvironmentBackend >> removeClassTag: aSymbol from: anRGPackage [

	^ anRGPackage pvtRemoveClassTag: aSymbol
]

{ #category : #class }
RGEnvironmentBackend >> removeClassVariable: anRGInstanceVariableDefinition from: anRGBehavior [

	^ anRGBehavior behaviorStrategy pvtRemoveClassVariable: anRGInstanceVariableDefinition
]

{ #category : #package }
RGEnvironmentBackend >> removeDefinedBehavior: anRGBehavior from: anRGPackage [

	^ anRGPackage pvtRemoveDefinedBehavior: anRGBehavior
]

{ #category : #'trait exclusion' }
RGEnvironmentBackend >> removeExclusion: aSymbol from: anRGTraitExclusion [

	^ anRGTraitExclusion pvtRemoveExclusion: aSymbol
]

{ #category : #package }
RGEnvironmentBackend >> removeExtensionMethod: anRGMethod from: anRGPackage [

	^ anRGPackage pvtRemoveExtensionMethod: anRGMethod
]

{ #category : #environment }
RGEnvironmentBackend >> removeGlobalVariable: anRGGlobalVariable from: anRGEnvironment [

	^ anRGEnvironment pvtRemoveGlobalVariable: anRGGlobalVariable
]

{ #category : #behavior }
RGEnvironmentBackend >> removeLocalMethod: anRGMethod from: anRGBehavior [

	^ anRGBehavior pvtRemoveLocalMethod: anRGMethod
]

{ #category : #class }
RGEnvironmentBackend >> removeMethodTag: aSymbol from: anRGBehavior [

	^ anRGBehavior pvtRemoveMethodTag: aSymbol
]

{ #category : #environment }
RGEnvironmentBackend >> removePackage: anRGPackage from: anRGEnvironment [

	^ anRGEnvironment pvtRemovePackage: anRGPackage
]

{ #category : #class }
RGEnvironmentBackend >> removeSharedPool: anRGPoolVariable from: anRGBehavior [

	^ anRGBehavior behaviorStrategy pvtRemoveSharedPool: anRGPoolVariable
]

{ #category : #'pointer layout' }
RGEnvironmentBackend >> removeSlot: anRGSlot from: anRGLayout [

	^ anRGLayout pvtRemoveSlot: anRGSlot
]

{ #category : #'trait composition' }
RGEnvironmentBackend >> removeTransformation: anRGTraitTransformation from: anRGTraitComposition [

	^ anRGTraitComposition pvtRemoveTransformation: anRGTraitTransformation
]

{ #category : #resolving }
RGEnvironmentBackend >> resolvedPropertiesFor: anRGObject [

	^ anRGObject pvtResolvedProperties
]

{ #category : #method }
RGEnvironmentBackend >> setAuthorFor: anRGMethod to: aDateAndTime [

	^  anRGMethod pvtAuthor: aDateAndTime
]

{ #category : #metaclass }
RGEnvironmentBackend >> setBaseClassFor: anRGBehavior to: anRGClass [

	^  anRGBehavior behaviorStrategy pvtBaseClass: anRGClass
]

{ #category : #'class comment' }
RGEnvironmentBackend >> setClassCommentAuthorFor: anRGComment to: aString [

	^  anRGComment pvtAuthor: aString
]

{ #category : #'class comment' }
RGEnvironmentBackend >> setClassCommentContentFor: anRGComment to: anObject [

	anRGComment pvtContent: anObject.

]

{ #category : #'class comment' }
RGEnvironmentBackend >> setClassCommentTimeFor: anRGComment to: aDateAndTime [

	^  anRGComment pvtTime: aDateAndTime
]

{ #category : #trait }
RGEnvironmentBackend >> setClassTraitFor: anRGTrait to: anRGMetatraitDefinition [

	^  anRGTrait behaviorStrategy pvtClassTrait: anRGMetatraitDefinition
]

{ #category : #class }
RGEnvironmentBackend >> setCommentFor: anRGBehavior to: anRGComment [

	^  anRGBehavior behaviorStrategy pvtComment: anRGComment
]

{ #category : #slot }
RGEnvironmentBackend >> setExpressionFor: anRGUnknownSlot to: aString [

	^  anRGUnknownSlot pvtExpression: aString
]

{ #category : #behavior }
RGEnvironmentBackend >> setLayoutFor: anRGBehavior to: anRGLayout [

	^  anRGBehavior behaviorStrategy pvtLayout: anRGLayout
]

{ #category : #'as yet unclassified' }
RGEnvironmentBackend >> setMetaclassFor: anRGBehavior to: anRGMetaclass [

	^  anRGBehavior pvtMetaclass: anRGMetaclass
]

{ #category : #'metaclass trait' }
RGEnvironmentBackend >> setMetaclassTraitBaseTraitFor: anRGMetaclassTrait to: anRGTrait [

	^  anRGMetaclassTrait pvtBaseTrait: anRGTrait
]

{ #category : #method }
RGEnvironmentBackend >> setMethodPackageFor: anRGMethod to: anRGPackage [

	^  anRGMethod pvtPackage: anRGPackage
]

{ #category : #general }
RGEnvironmentBackend >> setNameFor: anRGObject to: aString [

	^ anRGObject pvtName: aString
]

{ #category : #class }
RGEnvironmentBackend >> setPackageFor: anRGBehavior to: anRGPackage [

	^  anRGBehavior behaviorStrategy pvtPackage: anRGPackage
]

{ #category : #method }
RGEnvironmentBackend >> setSourceCodeFor: anRGMethod to: anObject [

	anRGMethod pvtSourceCode: anObject
]

{ #category : #'trait transormation' }
RGEnvironmentBackend >> setSubjectFor: anRGTraitComposition to: anRGTrait [

	^  anRGTraitComposition pvtSubject: anRGTrait
]

{ #category : #behavior }
RGEnvironmentBackend >> setSuperclassFor: anRGBehavior to: anObject [

	^  anRGBehavior pvtSuperclass: anObject
]

{ #category : #method }
RGEnvironmentBackend >> setTimeFor: anRGMethod to: aDateAndTime [

	^  anRGMethod pvtTime: aDateAndTime
]

{ #category : #trait }
RGEnvironmentBackend >> setTraitCommentFor: anRGBehavior to: anRGComment [

	^  anRGBehavior behaviorStrategy pvtComment: anRGComment
]

{ #category : #behavior }
RGEnvironmentBackend >> setTraitCompositionFor: anRGBehavior to: anRGTraitComposition [

	^  anRGBehavior pvtTraitComposition: anRGTraitComposition
]

{ #category : #trait }
RGEnvironmentBackend >> setTraitPackageFor: anRGBehavior to: anRGPackage [

	^  anRGBehavior pvtPackage: anRGPackage
]

{ #category : #class }
RGEnvironmentBackend >> sharedPoolsFor: anRGBehavior do: aBlock [

	^ anRGBehavior behaviorStrategy pvtSharedPoolsDo: aBlock
]

{ #category : #'pointer layout' }
RGEnvironmentBackend >> slotsFor: anRGLayout do: aBlock [

	^ anRGLayout pvtSlotsDo: aBlock
]

{ #category : #method }
RGEnvironmentBackend >> sourceCodeFor: anRGMethod [ 

	^ anRGMethod pvtSafeSourceCode

]

{ #category : #'trait transormation' }
RGEnvironmentBackend >> subjectFor: anRGTraitComposition [

	^  anRGTraitComposition pvtSubject
]

{ #category : #behavior }
RGEnvironmentBackend >> superclassFor: anRGBehavior [

	^  anRGBehavior pvtSuperclass
]

{ #category : #class }
RGEnvironmentBackend >> tagClass: anRGBehavior with: aSymbol [

	^ anRGBehavior pvtTagWith: aSymbol
]

{ #category : #method }
RGEnvironmentBackend >> tagMethod: anRGMethod with: aSymbol [

	^ anRGMethod pvtTagWith: aSymbol
]

{ #category : #class }
RGEnvironmentBackend >> tagsForClass: anRGBehavior do: aBlock [

	^  anRGBehavior pvtTagsDo: aBlock
]

{ #category : #package }
RGEnvironmentBackend >> tagsForClassesFor: anRGPackage do: aBlock [

	^  anRGPackage pvtTagsForClassesDo: aBlock
]

{ #category : #method }
RGEnvironmentBackend >> tagsForMethod: anRGMethod do: aBlock [

	^  anRGMethod pvtTagsDo: aBlock
]

{ #category : #class }
RGEnvironmentBackend >> tagsForMethodsFor: anRGBehavior do: aBlock [

	^  anRGBehavior pvtTagsForMethodsDo: aBlock
]

{ #category : #method }
RGEnvironmentBackend >> timeFor: anRGMethod [

	^  anRGMethod pvtTime
]

{ #category : #trait }
RGEnvironmentBackend >> traitCommentFor: anRGBehavior [

	^  anRGBehavior behaviorStrategy pvtComment
]

{ #category : #behavior }
RGEnvironmentBackend >> traitCompositionFor: anRGBehavior [

	^  anRGBehavior pvtTraitComposition
]

{ #category : #trait }
RGEnvironmentBackend >> traitPackageFor: anRGBehavior [

	^  anRGBehavior behaviorStrategy pvtPackage
]

{ #category : #'trait composition' }
RGEnvironmentBackend >> transformationsFor: anRGTraitComposition do: aBlock [

	^ anRGTraitComposition pvtTransformationsDo: aBlock
]

{ #category : #resolving }
RGEnvironmentBackend >> unresolvedPropertiesFor: anRGObject [

	^ anRGObject pvtUnresolvedProperties
]

{ #category : #class }
RGEnvironmentBackend >> untagClass: anRGBehavior from: aSymbol [

	^ anRGBehavior pvtUntagFrom: aSymbol
]

{ #category : #method }
RGEnvironmentBackend >> untagMethod: anRGMethod from: aSymbol [

	^ anRGMethod pvtUntagFrom: aSymbol
	

]
