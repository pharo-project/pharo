Class {
	#name : #RG2ReadOnlyImageBackend,
	#superclass : #RG2ReadOnlyBackend,
	#category : #'Ring2-Core-Environment'
}

{ #category : #'as yet unclassified' }
RG2ReadOnlyImageBackend class >> for: aRG2Environment [

	^ self new 
		environment: aRG2Environment;
		yourself.
]

{ #category : #'trait alias' }
RG2ReadOnlyImageBackend >> aliasesFor: aRG2TraitAliasDefinition do: aBlock [

	^ (aRG2TraitAliasDefinition propertyNamed: #realObject ifAbsent: [ self error: 'You can use only trait transformations generated directly by this backend' ]) aliases do: [:each | aBlock value: each ]
]

{ #category : #method }
RG2ReadOnlyImageBackend >> astFor: aRG2MethodDefinition [

	^  aRG2MethodDefinition astFromSource
]

{ #category : #method }
RG2ReadOnlyImageBackend >> authorFor: aRG2MethodDefinition [

	^ aRG2MethodDefinition authorForStamp: (self realMethodFor: aRG2MethodDefinition) stamp
]

{ #category : #metaclass }
RG2ReadOnlyImageBackend >> baseClassFor: aRG2MetaclassTraitDefinition [

	^ (self realBehaviorFor: aRG2MetaclassTraitDefinition) baseClass asRing2MinimalDefinitionIn: aRG2MetaclassTraitDefinition environment.
]

{ #category : #environment }
RG2ReadOnlyImageBackend >> behaviorsFor: aRG2Environment do: aBlock [

	SystemNavigation default allBehaviors do: [:each |
		| def | 
		def := self definitionFor: each ifAbsentRegister: [each asRing2MinimalDefinitionIn: aRG2Environment].
		aBlock value: def.].
]

{ #category : #class }
RG2ReadOnlyImageBackend >> categoryFor: aRG2BehaviorDefinition [

	^ (self realBehaviorFor: aRG2BehaviorDefinition) category

]

{ #category : #'class comment' }
RG2ReadOnlyImageBackend >> classCommentAuthorFor: aRG2CommentDefinition [

	^ aRG2CommentDefinition authorForStamp: (self realBehaviorFor: aRG2CommentDefinition parent) organization commentStamp.

]

{ #category : #'class comment' }
RG2ReadOnlyImageBackend >> classCommentContentFor: aRG2CommentDefinition [

	^ (self realBehaviorFor: aRG2CommentDefinition parent) organization classComment.

]

{ #category : #'class comment' }
RG2ReadOnlyImageBackend >> classCommentTimeFor: aRG2CommentDefinition [

	^ aRG2CommentDefinition timeForStamp: (self realBehaviorFor: aRG2CommentDefinition parent) organization commentStamp.
]

{ #category : #trait }
RG2ReadOnlyImageBackend >> classTraitFor: aRG2TraitDefinition [

	^ (self realBehaviorFor: aRG2TraitDefinition) classTrait asRing2MinimalDefinitionIn: aRG2TraitDefinition environment.
]

{ #category : #class }
RG2ReadOnlyImageBackend >> classVariablesFor: aRG2BehaviorDefinition do: aBlock [

	| realClass |
	realClass := self realBehaviorFor: aRG2BehaviorDefinition.
	realClass classVariables do: [ :classVariable | 
		| def |
		def := RG2ClassVariableDefinition named: classVariable key parent: aRG2BehaviorDefinition.
		aBlock value: def. ]
]

{ #category : #class }
RG2ReadOnlyImageBackend >> commentFor: aRG2BehaviorDefinition [

	^  (self realBehaviorFor: aRG2BehaviorDefinition) asRing2MinimalCommentDefinitionIn: self environment
]

{ #category : #'unresolved objects' }
RG2ReadOnlyImageBackend >> createUnresolvedClassGroupFor: aRG2BehaviorDefinition [ 

	^ 	aRG2BehaviorDefinition isMeta
		ifFalse: [ aRG2BehaviorDefinition ]
		ifTrue: [ aRG2BehaviorDefinition metaclass].

]

{ #category : #package }
RG2ReadOnlyImageBackend >> definedBehaviorsFor: aRG2PackageDefinition do: aBlock [

	| realPackage |
	
	realPackage := self realPackageFor: aRG2PackageDefinition.
	realPackage ifNotNil: [ 
		realPackage definedClassesDo: [:behaviorName |
			| def cls | 
			cls := Smalltalk classOrTraitNamed: behaviorName.
			def := self definitionFor: cls ifAbsentRegister: [cls asRing2MinimalDefinitionIn: aRG2PackageDefinition environment].
			aBlock value: def.]]
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyImageBackend >> definitionFor: anObject [ 

	^ self definitionFor: anObject ifAbsentRegister: [ anObject ensureRing2DefinitionIn: self environment ]

]

{ #category : #'trait exclusion' }
RG2ReadOnlyImageBackend >> exclusionsFor: aRG2TraitExclusionDefinition do: aBlock [

	^ (aRG2TraitExclusionDefinition propertyNamed: #realObject ifAbsent: [ self error: 'You can use only trait transformations generated directly by this backend' ]) exclusions do: [:each | aBlock value: each ]
]

{ #category : #slot }
RG2ReadOnlyImageBackend >> expressionFor: aRG2UnknownSlotDefinition [

	| realClass realSlot |
	
	realClass := self realBehaviorFor: aRG2UnknownSlotDefinition parent parent.
	realSlot := realClass slotNamed: aRG2UnknownSlotDefinition name.
	^ (realSlot printString copyAfter: $>) trimBoth

]

{ #category : #package }
RG2ReadOnlyImageBackend >> extensionMethodsFor: aRG2PackageDefinition do: aBlock [

	| realPackage |
	
	realPackage := self realPackageFor: aRG2PackageDefinition.
	realPackage ifNotNil: [ 
		realPackage extensionMethods do: [:aCompiledMethod |
			| def | 
			def := self definitionFor: aCompiledMethod ifAbsentRegister: [aCompiledMethod asRing2MinimalDefinitionIn: aRG2PackageDefinition environment].
			aBlock value: def.]]
]

{ #category : #environment }
RG2ReadOnlyImageBackend >> globalVariablesFor: aRG2Environment do: aBlock [

	Smalltalk globals associations do: [:each |
		| def | 
		def := self definitionFor: each ifAbsentRegister: [each asRing2MinimalDefinitionIn: aRG2Environment].
		aBlock value: def.].
]

{ #category : #resolving }
RG2ReadOnlyImageBackend >> hasFullyResolved: aRG2Definition [

	^ true

]

{ #category : #resolving }
RG2ReadOnlyImageBackend >> hasFullyUnresolved: aRG2Definition [		
		
	^ false
]

{ #category : #resolving }
RG2ReadOnlyImageBackend >> hasResolved: aRG2Definition [

	^ true

]

{ #category : #method }
RG2ReadOnlyImageBackend >> hasSourceCodeFor: aRG2MethodDefinition [ 

	^ (self realMethodFor: aRG2MethodDefinition) sourceCode
]

{ #category : #initialization }
RG2ReadOnlyImageBackend >> initialize [

	super initialize.
	
	providedDefinitions := IdentityDictionary new.
	
]

{ #category : #behavior }
RG2ReadOnlyImageBackend >> layoutFor: aRG2BehaviorDefinition [

	| realClass realLayout def |
	
	realClass := self realBehaviorFor: aRG2BehaviorDefinition.
	realLayout := realClass classLayout.
	def := self 	definitionFor: realLayout ifAbsentRegister: [ 
			realLayout asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment ].
	^ def
]

{ #category : #behavior }
RG2ReadOnlyImageBackend >> localMethodsFor: aRG2BehaviorDefinition do: aBlock [

	(self realBehaviorFor: aRG2BehaviorDefinition) localMethods do: [:method |
		| def |
		def := self definitionFor: method ifAbsentRegister: [
			method asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment].
		aBlock value: def]

]

{ #category : #'class description' }
RG2ReadOnlyImageBackend >> metaClassFor: aRG2BehaviorDefinition [

	| realClass realMetaclass def |
	
	realClass := self realBehaviorFor: aRG2BehaviorDefinition.
	realMetaclass := realClass class.
	def := self 	definitionFor: realMetaclass ifAbsentRegister: [ 
			realMetaclass asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment ].
	^ def

]

{ #category : #behavior }
RG2ReadOnlyImageBackend >> metaclassFor: aRG2BehaviorDefinition [

	| realBehavior realMetaclass def |
	
	realBehavior := self realBehaviorFor: aRG2BehaviorDefinition.
	realMetaclass := realBehavior class.
	def := self 	definitionFor: realMetaclass ifAbsentRegister: [ 
			realMetaclass asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment ].
	^ def

]

{ #category : #'metaclass trait' }
RG2ReadOnlyImageBackend >> metaclassTraitBaseTraitFor: aRG2MetaclassTraitDefinition [

	^ (self realBehaviorFor: aRG2MetaclassTraitDefinition) baseTrait asRing2MinimalDefinitionIn: aRG2MetaclassTraitDefinition environment.
]

{ #category : #method }
RG2ReadOnlyImageBackend >> methodPackageFor: aRG2MethodDefinition [
	
	| realMethod |
	
	realMethod := self realMethodFor: aRG2MethodDefinition.
	^ realMethod 
		ifNotNil: [ realMethod package ifNotNil: [ :aPackage | 
				aPackage asRing2MinimalDefinitionIn: aRG2MethodDefinition environment ] ]
]

{ #category : #general }
RG2ReadOnlyImageBackend >> nameFor: aRG2Definition [

	^ aRG2Definition pvtName
]

{ #category : #class }
RG2ReadOnlyImageBackend >> packageFor: aRG2BehaviorDefinition [

	^ (self realBehaviorFor: aRG2BehaviorDefinition) package asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment.
]

{ #category : #environment }
RG2ReadOnlyImageBackend >> packagesFor: aRG2Environment do: aBlock [

	RPackageOrganizer default packagesDo: [:each |
		| def | 
		def := self definitionFor: each ifAbsentRegister: [each asRing2MinimalDefinitionIn: aRG2Environment].
		aBlock value: def.].
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyImageBackend >> realBehaviorFor: aRG2BehaviorDefinition [

	"TODO: should we cache the definition?"
	^ Smalltalk classOrTraitNamed: aRG2BehaviorDefinition name asSymbol
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyImageBackend >> realMethodFor: aRG2MethodDefinition [

	"TODO: should we cache the definition?"
	^ ((self realBehaviorFor: aRG2MethodDefinition parent) >> aRG2MethodDefinition selector)
	

]

{ #category : #'as yet unclassified' }
RG2ReadOnlyImageBackend >> realPackageFor: aRG2PackageDefinition [

	^ RPackageOrganizer default packageNamed: aRG2PackageDefinition name ifAbsent: [nil].
]

{ #category : #resolving }
RG2ReadOnlyImageBackend >> resolvedPropertiesFor: aRG2Definition [

	^ aRG2Definition pvtResolvableProperties collect: #key
]

{ #category : #class }
RG2ReadOnlyImageBackend >> sharedPoolsFor: aRG2Behavior do: aBlock [

	| realClass |
	realClass := self realBehaviorFor: aRG2Behavior.
	realClass sharedPools do: [ :each | 
			| def | 
			def := self 
				definitionFor: each
				ifAbsentRegister: [ each asRing2MinimalDefinitionIn: aRG2Behavior environment ].
			aBlock value: def ]
]

{ #category : #'pointer layout' }
RG2ReadOnlyImageBackend >> slotsFor: aRG2LayoutDefinition do: aBlock [
	| realClass |
	realClass := self realBehaviorFor: aRG2LayoutDefinition parent.
	realClass classLayout slots
		do: [ :each | 
			| def |
			def := self
				definitionFor: each
				ifAbsentRegister: [ each asRing2MinimalDefinitionIn: aRG2LayoutDefinition environment ].
			aBlock value: def ]
]

{ #category : #method }
RG2ReadOnlyImageBackend >> sourceCodeFor: aRG2MethodDefinition [ 

	^ (self realMethodFor: aRG2MethodDefinition) sourceCode

]

{ #category : #'trait transormation' }
RG2ReadOnlyImageBackend >> subjectFor: aRG2TraitTransformationDefinition [

	^ (aRG2TraitTransformationDefinition propertyNamed: #realObject ifAbsent: [ self error: 'You can use only trait transformations generated directly by this backend' ]) subject asRing2MinimalDefinitionIn: aRG2TraitTransformationDefinition environment
]

{ #category : #behavior }
RG2ReadOnlyImageBackend >> superclassFor: aRG2BehaviorDefinition [

	| realClass realSuperclass def |
	
	realClass := self realBehaviorFor: aRG2BehaviorDefinition.
	realSuperclass := realClass superclass.
	realSuperclass ifNil: [ realSuperclass := realClass ].
	def := self 	definitionFor: realSuperclass ifAbsentRegister: [ 
			realSuperclass asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment ].
	^ def

]

{ #category : #behavior }
RG2ReadOnlyImageBackend >> tagsForClass: aRG2BehaviorDefinition do: aBlock [

	| realClass |
	
	realClass := self realBehaviorFor: aRG2BehaviorDefinition.
	realClass tags do: [:tag | aBlock value: tag ]

]

{ #category : #method }
RG2ReadOnlyImageBackend >> tagsForMethod: aRG2MethodDefinition do: aBlock [

	| realMethod |
	
	realMethod := self realMethodFor: aRG2MethodDefinition.
	{ realMethod protocol } do: aBlock.
]

{ #category : #method }
RG2ReadOnlyImageBackend >> tagsForMethodsFor: aRG2BehaviorDefinition do: aBlock [

	(self realBehaviorFor: aRG2BehaviorDefinition) protocols do: aBlock

]

{ #category : #method }
RG2ReadOnlyImageBackend >> timeFor: aRG2MethodDefinition [

	^ aRG2MethodDefinition timeForStamp: (self realMethodFor: aRG2MethodDefinition) stamp
]

{ #category : #trait }
RG2ReadOnlyImageBackend >> traitCommentFor: aRG2BehaviorDefinition [

	^  (self realBehaviorFor: aRG2BehaviorDefinition) asRing2MinimalCommentDefinitionIn: self environment
]

{ #category : #behavior }
RG2ReadOnlyImageBackend >> traitCompositionFor: aRG2BehaviorDefinition [

	| realClass realTraitComposition def |
	
	realClass := self realBehaviorFor: aRG2BehaviorDefinition.
	realTraitComposition := realClass traitComposition.
	def := self 	definitionFor: realTraitComposition ifAbsentRegister: [ 
			realTraitComposition asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment ].
	^ def

]

{ #category : #trait }
RG2ReadOnlyImageBackend >> traitPackageFor: aRG2BehaviorDefinition [

	^ (self realBehaviorFor: aRG2BehaviorDefinition) package asRing2MinimalDefinitionIn: aRG2BehaviorDefinition environment.
]

{ #category : #'trait composition' }
RG2ReadOnlyImageBackend >> transformationsFor: aRG2TraitCompositionDefinition do: aBlock [

	| realBehavior realTraitComposition |
	"^ aRG2TraitCompositionDefinition pvtTransformationsDo: aBlock"

	realBehavior := self realBehaviorFor: aRG2TraitCompositionDefinition parent.
	realTraitComposition := realBehavior traitComposition.

	realTraitComposition transformations do: [:transformation |
		| def |
		def := self definitionFor: transformation ifAbsentRegister: [
			transformation asRing2MinimalDefinitionIn: aRG2TraitCompositionDefinition environment].
		aBlock value: def]

]

{ #category : #resolving }
RG2ReadOnlyImageBackend >> unresolvedPropertiesFor: aRG2Definition [

	^ Dictionary new.
]
