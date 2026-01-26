Extension { #name : 'RGBehavior' }

{ #category : '*Calypso-Ring' }
RGBehavior >> calypsoEnvironmentType [
	^ClyClass
]

{ #category : '*Calypso-Ring' }
RGBehavior >> includesMethodsAffectedBy: aSystemAnnouncement [
	^aSystemAnnouncement affectsMethodsDefinedInClass: self
]

{ #category : '*Calypso-Ring' }
RGBehavior >> includesVariablesAffectedByModificationOf: modifiedClass [
	"Any class has ClassVariables which are visible from instance side and class side. That's why here we must check both class and metaclass"

	self instanceSide = modifiedClass instanceSide ifTrue: [ ^true ].
	self classSide = modifiedClass classSide ifTrue: [ ^true ].

	self isRootInEnvironment ifTrue: [ ^false ].

	^self superclass includesVariablesAffectedByModificationOf: modifiedClass
]

{ #category : '*Calypso-Ring' }
RGBehavior >> metaLevelScope [
	^self isInstanceSide ifTrue: [ ClyInstanceSideScope ] ifFalse: [ ClyClassSideScope ]
]

{ #category : '*Calypso-Ring' }
RGBehavior >> subclassDefinerClass [
	^RGClassDefinitionCompiler on: self environment
]

{ #category : '*Calypso-Ring' }
RGBehavior >> traitCompositionIncludes: aTrait [
	^self == aTrait or:
		[self hasTraitComposition and:
			[self traitComposition allTraits includes: aTrait]]
]

{ #category : '*Calypso-Ring' }
RGBehavior >> visibleMethods [
	"Answer a <Collection> of <RGMethod> in the receiver"

	^ self methods
]

{ #category : '*Calypso-Ring' }
RGBehavior >> withAllSubclasses [

	^ self subclasses asOrderedCollection
		add: self;
		yourself
]
