Class {
	#name : #RGReadOnlyImageBackend,
	#superclass : #RGReadOnlyBackend,
	#category : #'Ring-Core-Environment'
}

{ #category : #'trait alias' }
RGReadOnlyImageBackend >> aliasesFor: anRGTraitAlias do: aBlock [

	^ (anRGTraitAlias propertyNamed: #realObject ifAbsent: [ self error: 'You can use only trait transformations generated directly by this backend' ]) aliases associations do: [:each | aBlock value: each ]
]

{ #category : #method }
RGReadOnlyImageBackend >> authorFor: anRGMethod [

	^ RGStampParser authorForStamp: (self realMethodFor: anRGMethod) stamp
]

{ #category : #metaclass }
RGReadOnlyImageBackend >> baseClassFor: anRGMetaclassTrait [

	^ (self realBehaviorFor: anRGMetaclassTrait) baseClass asRingMinimalDefinitionIn: anRGMetaclassTrait environment
]

{ #category : #environment }
RGReadOnlyImageBackend >> behaviorsFor: anRGEnvironment do: aBlock [

	SystemNavigation default allBehaviors do: [:each |
		| def |
		def := self definitionFor: each ifAbsentRegister: [each asRingMinimalDefinitionIn: anRGEnvironment].
		aBlock value: def.]
]

{ #category : #class }
RGReadOnlyImageBackend >> categoryFor: anRGBehavior [

	^ (self realBehaviorFor: anRGBehavior) category
]

{ #category : #'class comment' }
RGReadOnlyImageBackend >> classCommentAuthorFor: anRGComment [

	^ RGStampParser authorForStamp: (self realBehaviorFor: anRGComment parent) commentStamp
]

{ #category : #'class comment' }
RGReadOnlyImageBackend >> classCommentContentFor: anRGComment [

	^ (self realBehaviorFor: anRGComment parent) comment
]

{ #category : #'class comment' }
RGReadOnlyImageBackend >> classCommentTimeFor: anRGComment [

	^ RGStampParser timeForStamp: (self realBehaviorFor: anRGComment parent) commentStamp
]

{ #category : #trait }
RGReadOnlyImageBackend >> classTraitFor: anRGTrait [

	^ (self realBehaviorFor: anRGTrait) classTrait asRingMinimalDefinitionIn: anRGTrait environment
]

{ #category : #class }
RGReadOnlyImageBackend >> classVariablesFor: anRGBehavior do: aBlock [

	| realClass |
	realClass := self realBehaviorFor: anRGBehavior.
	realClass classVariables do: [ :classVariable |
		| def |
		def := RGClassVariable named: classVariable key parent: anRGBehavior.
		aBlock value: def. ]
]

{ #category : #class }
RGReadOnlyImageBackend >> commentFor: anRGBehavior [

	^  (self realBehaviorFor: anRGBehavior) asRingMinimalCommentDefinitionIn: self environment
]

{ #category : #'unresolved objects' }
RGReadOnlyImageBackend >> createUnresolvedClassGroupFor: anRGBehavior [

	^ 	anRGBehavior isMeta
		ifFalse: [ anRGBehavior ]
		ifTrue: [ anRGBehavior metaclass]
]

{ #category : #package }
RGReadOnlyImageBackend >> definedBehaviorsFor: anRGPackage do: aBlock [

	| realPackage |

	realPackage := self realPackageFor: anRGPackage.
	realPackage ifNotNil: [
		realPackage definedClassesDo: [:behaviorName |
			| def cls |
			cls := Smalltalk classOrTraitNamed: behaviorName.
			def := self definitionFor: cls ifAbsentRegister: [cls asRingMinimalDefinitionIn: anRGPackage environment].
			aBlock value: def.]]
]

{ #category : #'trait exclusion' }
RGReadOnlyImageBackend >> exclusionsFor: anRGTraitExclusion do: aBlock [

	^ (anRGTraitExclusion propertyNamed: #realObject ifAbsent: [ self error: 'You can use only trait transformations generated directly by this backend' ]) exclusions do: [:each | aBlock value: each ]
]

{ #category : #slot }
RGReadOnlyImageBackend >> expressionFor: anRGUnknownSlot [

	| realClass realSlot |

	realClass := self realBehaviorFor: anRGUnknownSlot parent parent.
	realSlot := realClass slotNamed: anRGUnknownSlot name.
	^ (realSlot printString copyAfter: $>) trimBoth
]

{ #category : #package }
RGReadOnlyImageBackend >> extensionMethodsFor: anRGPackage do: aBlock [

	| realPackage |

	realPackage := self realPackageFor: anRGPackage.
	realPackage ifNotNil: [
		realPackage extensionMethods do: [:aCompiledMethod |
			| def |
			def := self definitionFor: aCompiledMethod ifAbsentRegister: [aCompiledMethod asRingMinimalDefinitionIn: anRGPackage environment].
			aBlock value: def.]]
]

{ #category : #environment }
RGReadOnlyImageBackend >> globalVariablesFor: anRGEnvironment do: aBlock [

	Smalltalk globals associations do: [:each |
		| def |
		def := self definitionFor: each ifAbsentRegister: [each asRingMinimalDefinitionIn: anRGEnvironment].
		aBlock value: def.]
]

{ #category : #resolving }
RGReadOnlyImageBackend >> hasFullyResolved: anRGObject [

	^ true
]

{ #category : #resolving }
RGReadOnlyImageBackend >> hasFullyUnresolved: anRGObject [

	^ false
]

{ #category : #resolving }
RGReadOnlyImageBackend >> hasResolved: anRGObject [

	^ true
]

{ #category : #method }
RGReadOnlyImageBackend >> hasSourceCodeFor: anRGMethod [

	^ (self realMethodFor: anRGMethod) sourceCode
]

{ #category : #behavior }
RGReadOnlyImageBackend >> layoutFor: anRGBehavior [

	| realClass realLayout def |

	realClass := self realBehaviorFor: anRGBehavior.
	realLayout := realClass classLayout.
	def := self 	definitionFor: realLayout ifAbsentRegister: [
			realLayout asRingMinimalDefinitionIn: anRGBehavior environment ].
	^ def
]

{ #category : #behavior }
RGReadOnlyImageBackend >> localMethodsFor: anRGBehavior do: aBlock [

	(self realBehaviorFor: anRGBehavior) localMethods do: [:method |
		| def |
		def := self definitionFor: method ifAbsentRegister: [
			method asRingMinimalDefinitionIn: anRGBehavior environment].
		aBlock value: def]
]

{ #category : #'class description' }
RGReadOnlyImageBackend >> metaClassFor: anRGBehavior [

	| realClass realMetaclass def |

	realClass := self realBehaviorFor: anRGBehavior.
	realMetaclass := realClass class.
	def := self 	definitionFor: realMetaclass ifAbsentRegister: [
			realMetaclass asRingMinimalDefinitionIn: anRGBehavior environment ].
	^ def
]

{ #category : #behavior }
RGReadOnlyImageBackend >> metaclassFor: anRGBehavior [

	| realBehavior realMetaclass def |

	realBehavior := self realBehaviorFor: anRGBehavior.
	realMetaclass := realBehavior class.
	def := self 	definitionFor: realMetaclass ifAbsentRegister: [
			realMetaclass asRingMinimalDefinitionIn: anRGBehavior environment ].
	^ def
]

{ #category : #'metaclass trait' }
RGReadOnlyImageBackend >> metaclassTraitBaseTraitFor: anRGMetaclassTrait [
	^ (self realBehaviorFor: anRGMetaclassTrait) instanceSide
		asRingMinimalDefinitionIn: anRGMetaclassTrait environment
]

{ #category : #method }
RGReadOnlyImageBackend >> methodPackageFor: anRGMethod [

	^ (self realMethodFor: anRGMethod) package asRingMinimalDefinitionIn: anRGMethod environment
]

{ #category : #class }
RGReadOnlyImageBackend >> packageFor: anRGBehavior [

	^ (self realBehaviorFor: anRGBehavior) package asRingMinimalDefinitionIn: anRGBehavior environment
]

{ #category : #environment }
RGReadOnlyImageBackend >> packagesFor: anRGEnvironment do: aBlock [

	RPackageOrganizer default packagesDo: [:each |
		| def |
		def := self definitionFor: each ifAbsentRegister: [each asRingMinimalDefinitionIn: anRGEnvironment].
		aBlock value: def.]
]

{ #category : #accessing }
RGReadOnlyImageBackend >> realBehaviorFor: anRGBehavior [

	"TODO: should we cache the definition?"
	^ Smalltalk classOrTraitNamed: anRGBehavior name asSymbol
]

{ #category : #accessing }
RGReadOnlyImageBackend >> realMethodFor: anRGMethod [

	"TODO: should we cache the definition?"
	^ ((self realBehaviorFor: anRGMethod parent) >> anRGMethod selector)
]

{ #category : #accessing }
RGReadOnlyImageBackend >> realPackageFor: anRGPackage [

	^ RPackageOrganizer default packageNamed: anRGPackage name ifAbsent: [nil]
]

{ #category : #resolving }
RGReadOnlyImageBackend >> resolvedPropertiesFor: anRGObject [

	^ anRGObject pvtResolvableProperties collect: [:each | each key]
]

{ #category : #'pointer layout' }
RGReadOnlyImageBackend >> slotsFor: anRGLayout do: aBlock [
	| realClass |
	realClass := self realBehaviorFor: anRGLayout parent.
	realClass classLayout slots
		do: [ :each |
			| def |
			def := self
				definitionFor: each
				ifAbsentRegister: [ each asRingMinimalDefinitionIn: anRGLayout environment ].
			aBlock value: def ]
]

{ #category : #method }
RGReadOnlyImageBackend >> sourceCodeFor: anRGMethod [

	^ (self realMethodFor: anRGMethod) sourceCode
]

{ #category : #'trait transormation' }
RGReadOnlyImageBackend >> subjectFor: anRGTraitTransformation [

	^ (anRGTraitTransformation propertyNamed: #realObject ifAbsent: [ self error: 'You can use only trait transformations generated directly by this backend' ]) subject asRingMinimalDefinitionIn: anRGTraitTransformation environment
]

{ #category : #behavior }
RGReadOnlyImageBackend >> superclassFor: anRGBehavior [

	| realClass realSuperclass def |

	realClass := self realBehaviorFor: anRGBehavior.
	realSuperclass := realClass superclass.
	realSuperclass ifNil: [ realSuperclass := realClass ].
	def := self 	definitionFor: realSuperclass ifAbsentRegister: [
			realSuperclass asRingMinimalDefinitionIn: anRGBehavior environment ].
	^ def
]

{ #category : #behavior }
RGReadOnlyImageBackend >> tagsForClass: anRGBehavior do: aBlock [

	| realClass |

	realClass := self realBehaviorFor: anRGBehavior.
	realClass tags do: [:tag | aBlock value: tag ]
]

{ #category : #method }
RGReadOnlyImageBackend >> tagsForMethod: anRGMethod do: aBlock [

	| realMethod |

	realMethod := self realMethodFor: anRGMethod.
	{ realMethod protocol } do: aBlock
]

{ #category : #method }
RGReadOnlyImageBackend >> tagsForMethodsFor: anRGBehavior do: aBlock [

	(self realBehaviorFor: anRGBehavior) protocolNames do: aBlock
]

{ #category : #method }
RGReadOnlyImageBackend >> timeFor: anRGMethod [

	^ RGStampParser timeForStamp: (self realMethodFor: anRGMethod) stamp
]

{ #category : #trait }
RGReadOnlyImageBackend >> traitCommentFor: anRGBehavior [

	^  (self realBehaviorFor: anRGBehavior) asRingMinimalCommentDefinitionIn: self environment
]

{ #category : #behavior }
RGReadOnlyImageBackend >> traitCompositionFor: anRGBehavior [

	| realClass realTraitComposition def |

	realClass := self realBehaviorFor: anRGBehavior.
	realTraitComposition := realClass traitComposition.
	def := self 	definitionFor: realTraitComposition ifAbsentRegister: [
			realTraitComposition asRingMinimalDefinitionIn: anRGBehavior environment ].
	^ def
]

{ #category : #trait }
RGReadOnlyImageBackend >> traitPackageFor: anRGBehavior [

	^ (self realBehaviorFor: anRGBehavior) package asRingMinimalDefinitionIn: anRGBehavior environment
]

{ #category : #'trait composition' }
RGReadOnlyImageBackend >> transformationsFor: anRGTraitComposition do: aBlock [

	| realBehavior realTraitComposition |
	"^ anRGTraitComposition pvtTransformationsDo: aBlock"

	realBehavior := self realBehaviorFor: anRGTraitComposition parent.
	realTraitComposition := realBehavior traitComposition.

	realTraitComposition transformations do: [:transformation |
		| def |
		def := self definitionFor: transformation ifAbsentRegister: [
			transformation asRingMinimalDefinitionIn: anRGTraitComposition environment].
		aBlock value: def]
]

{ #category : #resolving }
RGReadOnlyImageBackend >> unresolvedPropertiesFor: anRGObject [

	^ Dictionary new
]
