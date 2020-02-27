"
I am the base class for environments of the refactoring framework.

I define the common interface for all environments.
And I act as a factory for various specialized environments. See my 'environment' protocol.

I am used by different tools to create a 'views' of subsets of the whole system environment to browse or act on (searching/validations/refactoring)

create instances:
RBBrowserEnvironment new forClasses:  Number withAllSubclasses.
RBBrowserEnvironment new forPackageNames: { #Kernel }.

query:
|env|
env := RBBrowserEnvironment new forPackageNames: { #Kernel }.
env referencesTo:#asArray.
-> RBSelectorEnvironment.

browse:
|env|
env := RBBrowserEnvironment new forPackageNames: { #Kernel }.
(Smalltalk tools browser browsedEnvironment: env) open.

"
Class {
	#name : #RBBrowserEnvironment,
	#superclass : #Object,
	#instVars : [
		'label',
		'searchStrings'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #accessing }
RBBrowserEnvironment class >> default [
	^ RBBrowserEnvironment new
]

{ #category : #environments }
RBBrowserEnvironment >> & anEnvironment [ 
	"If we or anEnvironment includes everything, then just include the other environment (optimization)"

	self isSystem ifTrue: [^anEnvironment].
	anEnvironment isSystem ifTrue: [^self].
	^RBAndEnvironment onEnvironment: self and: anEnvironment
]

{ #category : #accessing }
RBBrowserEnvironment >> addSearchString: aString [

	searchStrings
		ifNil: [ searchStrings := SortedCollection
				sortBlock: [ :a :b | ( a indexOf: $: ifAbsent: [ a size ] ) > ( b indexOf: $: ifAbsent: [ b size ] ) ]
			].
	( searchStrings includes: aString )
		ifFalse: [ searchStrings add: aString ]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> allClasses [
	"Answer all the unique non-metaclasses of all the classes and metaclasses in this environment."

	| allClasses |
	allClasses := IdentitySet new: 4096.
	self classesDo: [ :each | 
		allClasses add: each instanceSide].
	^ allClasses asArray
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> allClassesAndTraits [
	" compatibility method with SystemDictionary "
	
	| allClassesAndTraits |
	allClassesAndTraits := OrderedCollection new: 4096.
	self allClassesAndTraitsDo: [ :each | 
		allClassesAndTraits add: each ].
	^ allClassesAndTraits asArray
]

{ #category : #private }
RBBrowserEnvironment >> allClassesAndTraitsDo: aBlock [
	self systemDictionary allClassesAndTraitsDo: aBlock
]

{ #category : #private }
RBBrowserEnvironment >> allClassesDo: aBlock [
	self systemDictionaryClassesDo: aBlock
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> allMetaClasses [
	"Answer all the unique non-metaclasses of all metaclasses in this environment."

	| allMetaClasses |
	allMetaClasses := OrderedCollection new: 4096.
	self
		classesDo: [ :each | 
			each isMeta
				ifTrue: [ allMetaClasses add: each instanceSide ] ].
	^ allMetaClasses asArray
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> allNonMetaClasses [
	"Answer all the unique non-metaclasses of all of all the non-metaclasses in this environment."
	| allNonMetaClasses |
	allNonMetaClasses := OrderedCollection new: 4096.
	self
		classesDo: [ :each | 
			each isMeta
				ifFalse: [ allNonMetaClasses add: each  ] ].
	^ allNonMetaClasses asArray
]

{ #category : #accessing }
RBBrowserEnvironment >> asSelectorEnvironment [
	^(RBClassEnvironment onEnvironment: self classes: self classes) asSelectorEnvironment
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> associationAt: aKey [
	^ self associationAt: aKey ifAbsent: [ self error: aKey printString , ' not found' ]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> associationAt: aKey ifAbsent: aBlock [
	| association class |
	association := self systemDictionary
		associationAt: aKey
		ifAbsent: [ ^ aBlock value ].
	class := association value isBehavior
		ifTrue: [ association value ]
		ifFalse: [ association value class ].
	^ ((self includesClass: class) or: [ self includesClass: class class ])
		ifTrue: [ association ]
		ifFalse: [ nil ]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> at: aKey [
	^ self at: aKey ifAbsent: [ self error: aKey printString , ' not found' ]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> at: aKey ifAbsent: aBlock [

	| association |

	association := self associationAt: aKey ifAbsent: [ nil ].
	^ association ifNil: [ aBlock value ] ifNotNil: [ association value ]
]

{ #category : #accessing }
RBBrowserEnvironment >> categories [
	^ self systemDictionary organization categories select: [ :each | self includesCategory: each ]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> classNames [
	| names |
	names := IdentitySet new: 4096.
	self classesDo: [ :each | names add: each instanceSide name ].
	^ names
]

{ #category : #accessing }
RBBrowserEnvironment >> classNamesFor: aCategoryName [
	^ (self systemDictionary organization listAtCategoryNamed: aCategoryName) select: [ :each |
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		class notNil 
			and: [ (self includesClass: class)
			or: [ self includesClass: class class ] ] ]
]

{ #category : #environments }
RBBrowserEnvironment >> classVarRefsTo: instVarName in: aClass [ 
	^ RBVariableEnvironment 
		on: self
		referencesToClassVariable: instVarName
		in: aClass
]

{ #category : #accessing }
RBBrowserEnvironment >> classVariablesFor: aClass [ 
	^aClass classVarNames
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> classes [
	| classes |
	classes := IdentitySet new: 4096.
	self classesDo: [ :each | classes add: each ].
	^ classes
]

{ #category : #accessing }
RBBrowserEnvironment >> classesAndSelectorsDo: aBlock [ 
	self classesDo: [ :class | 
		self selectorsForClass: class do: [ :sel | 
			aBlock value: class value: sel ]]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> classesAndTraits [
	
	| classesAndTraits |
	classesAndTraits := OrderedCollection new: 4096.
	self classesAndTraitsDo: [ :each | 
		classesAndTraits add: each ].
	^ classesAndTraits asArray
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> classesAndTraitsDo: aBlock [ 
	self allClassesAndTraitsDo: [ :each |
		(self includesClass: each)
			ifTrue: [ aBlock value: each ] ]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> classesDo: aBlock [ 
	self systemDictionaryClassesDo: [ :each |
		(self includesClass: each)
			ifTrue: [aBlock value: each ] ]
]

{ #category : #copying }
RBBrowserEnvironment >> copyEmpty [
	^ self class new
]

{ #category : #private }
RBBrowserEnvironment >> defaultLabel [
	^ 'Smalltalk'
]

{ #category : #testing }
RBBrowserEnvironment >> definesClass: aClass [
	^ true
]

{ #category : #private }
RBBrowserEnvironment >> environment [
	^ self
]

{ #category : #environments }
RBBrowserEnvironment >> forCategories: categoryList [ 
	^RBCategoryEnvironment onEnvironment: self categories: categoryList
]

{ #category : #environments }
RBBrowserEnvironment >> forClass: aClass protocols: protocolCollection [ 
	^RBProtocolEnvironment onEnvironment: self
		class: aClass
		protocols: protocolCollection
]

{ #category : #environments }
RBBrowserEnvironment >> forClass: aClass selectors: selectorCollection [ 
	^(RBSelectorEnvironment 
		onMethods: selectorCollection
		forClass: aClass
		in: self)
		label: aClass name , '>>' 
					, (selectorCollection detect: [:each | true] ifNone: ['']);
		yourself
]

{ #category : #environments }
RBBrowserEnvironment >> forClasses: classCollection [ 
	| classes |
	classes := OrderedCollection new: classCollection size * 2.
	classCollection do: 
			[:each | 
			classes add: each instanceSide; add: each classSide].
	^RBClassEnvironment onEnvironment: self classes: classes
]

{ #category : #environments }
RBBrowserEnvironment >> forPackageNames: aCollection [
	^ RBPackageEnvironment onEnvironment: self packageNames: aCollection
]

{ #category : #environments }
RBBrowserEnvironment >> forPackages: aCollection [
	^ RBPackageEnvironment onEnvironment: self packages: aCollection
]

{ #category : #environments }
RBBrowserEnvironment >> forPragmas: aKeywordCollection [
	^ RBPragmaEnvironment onEnvironment: self keywords: aKeywordCollection
]

{ #category : #environments }
RBBrowserEnvironment >> implementorsMatching: aString [ 
	^RBSelectorEnvironment implementorsMatching: aString in: self
]

{ #category : #environments }
RBBrowserEnvironment >> implementorsOf: aSelector [ 
	^RBSelectorEnvironment implementorsOf: aSelector in: self
]

{ #category : #testing }
RBBrowserEnvironment >> includesCategory: aCategory [
	^ true
]

{ #category : #testing }
RBBrowserEnvironment >> includesClass: aClass [
	^ true
]

{ #category : #testing }
RBBrowserEnvironment >> includesMethod: aMethod [
	^ self 
		includesSelector: aMethod selector
		in: aMethod methodClass
]

{ #category : #testing }
RBBrowserEnvironment >> includesPackage: packageName [
	^ true
]

{ #category : #testing }
RBBrowserEnvironment >> includesProtocol: aProtocol in: aClass [
	^ true
]

{ #category : #testing }
RBBrowserEnvironment >> includesSelector: aSelector in: aClass [
	^ true
]

{ #category : #environments }
RBBrowserEnvironment >> instVarReadersTo: instVarName in: aClass [ 
	^RBVariableEnvironment 
		on: self
		readersOfInstanceVariable: instVarName
		in: aClass
]

{ #category : #environments }
RBBrowserEnvironment >> instVarRefsTo: instVarName in: aClass [ 
	^RBVariableEnvironment 
		on: self
		referencesToInstanceVariable: instVarName
		in: aClass
]

{ #category : #environments }
RBBrowserEnvironment >> instVarWritersTo: instVarName in: aClass [ 
	^RBVariableEnvironment 
		on: self
		writersOfInstanceVariable: instVarName
		in: aClass
]

{ #category : #accessing }
RBBrowserEnvironment >> instanceVariablesFor: aClass [ 
	^aClass instVarNames
]

{ #category : #testing }
RBBrowserEnvironment >> isClassEnvironment [
	^ false
]

{ #category : #testing }
RBBrowserEnvironment >> isEmpty [
	^ false
]

{ #category : #testing }
RBBrowserEnvironment >> isSelectorEnvironment [
	^ false
]

{ #category : #testing }
RBBrowserEnvironment >> isSystem [
	^ true
]

{ #category : #testing }
RBBrowserEnvironment >> isVariableEnvironment [
	^ false
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> keys [
	| keys |
	keys := Set new.
	self systemDictionary keysAndValuesDo: [ :key :value | 
		| class |
		value isBehavior ifTrue: [
			(self includesClass: value) 
				ifTrue: [ keys add: key ] ].
			class := value class.
			(self includesClass: class) 
				ifTrue: [ keys add: key ] ].
	^ keys
]

{ #category : #private }
RBBrowserEnvironment >> label [

	^ label ifNil: [ self defaultLabel ] ifNotNil: [ label ]
]

{ #category : #initialization }
RBBrowserEnvironment >> label: aString [
	label := aString
]

{ #category : #environments }
RBBrowserEnvironment >> matches: aString [ 
	^RBSelectorEnvironment matches: aString in: self
]

{ #category : #accessing }
RBBrowserEnvironment >> methods [
	| methods |
	methods := IdentitySet new: 4096.
	self methodsDo: [ :each | methods add: each ].
	^ methods
]

{ #category : #accessing }
RBBrowserEnvironment >> methodsDo: aBlock [
	
	self classesDo: [ :aClass |
		
		self selectorsForClass: aClass do: [ :selector |
			aBlock value: (aClass >> selector) ].
		self selectorsForClass: aClass class do: [ :selector |
			aBlock value: (aClass class >> selector) ] ]
]

{ #category : #environments }
RBBrowserEnvironment >> not [
	self isSystem ifTrue: [^RBSelectorEnvironment new].
	^RBNotEnvironment onEnvironment: self
]

{ #category : #accessing }
RBBrowserEnvironment >> numberClasses [
	^self classNames size
]

{ #category : #accessing }
RBBrowserEnvironment >> numberSelectors [
	| total |
	total := 0.
	self selectorsDo: [:sel | total := total + 1 ].
	^total
]

{ #category : #'accessing-packages' }
RBBrowserEnvironment >> packageAt: aName [
	^ self packageAt: aName ifAbsent: [ NotFound signalFor: aName in: self ]
]

{ #category : #'accessing-packages' }
RBBrowserEnvironment >> packageAt: aName ifAbsent: absentBlock [
	| package |
	package := self packageOrganizer 
		packageNamed: aName ifAbsent: absentBlock.
	
	^ (self includesCategory: aName)
		ifTrue: [ package ]
		ifFalse: absentBlock
]

{ #category : #private }
RBBrowserEnvironment >> packageOrganizer [

	^ RPackageOrganizer default
]

{ #category : #'accessing-packages' }
RBBrowserEnvironment >> packages [

	^ self packageOrganizer packages
		select: [ :package |
			self includesCategory: package name ]
]

{ #category : #'accessing-packages' }
RBBrowserEnvironment >> packagesWithContributors [
	"answer a list of all packages contributing to the definition of classes in the environment"
	^ self classes 
		flatCollect: #packages
		as: Set
]

{ #category : #printing }
RBBrowserEnvironment >> printOn: aStream [
	aStream nextPutAll: self label
]

{ #category : #accessing }
RBBrowserEnvironment >> problemCount [
	^self numberSelectors
]

{ #category : #accessing }
RBBrowserEnvironment >> protocolsFor: aClass [
	^aClass organization categories 
		select: [ :each | self includesProtocol: each in: aClass ]
]

{ #category : #environments }
RBBrowserEnvironment >> referencesTo: aLiteral [ 
	^RBSelectorEnvironment referencesTo: aLiteral in: self
]

{ #category : #environments }
RBBrowserEnvironment >> referencesTo: aLiteral in: aClass [ 
	| classes |
	classes := aClass withAllSuperclasses asSet.
	classes
		addAll: aClass allSubclasses;
		addAll: aClass class withAllSuperclasses;
		addAll: aClass class allSubclasses.
	^(self forClasses: classes) referencesTo: aLiteral
]

{ #category : #private }
RBBrowserEnvironment >> rootEnvironment [
	"The root environment representing everything."

	^ self
]

{ #category : #accessing }
RBBrowserEnvironment >> searchStrings [

	^ searchStrings ifNil: [ #() ] ifNotNil: [ searchStrings ]
]

{ #category : #accessing }
RBBrowserEnvironment >> searchStrings: aCollection [ 
	searchStrings := aCollection
]

{ #category : #environments }
RBBrowserEnvironment >> selectMethods: aBlock [ 
	| env |
	env := RBSelectorEnvironment onEnvironment: self.
	self classesAndSelectorsDo: 
			[:each :sel | 
			(aBlock value: (each compiledMethodAt: sel)) 
				ifTrue: [env addClass: each selector: sel]].
	^env
]

{ #category : #accessing }
RBBrowserEnvironment >> selectionIntervalFor: aString [ 
	| interval |
	self searchStrings ifEmpty: [^nil].
	interval := self selectionParseTreeIntervalFor: aString.
	interval ifNotNil: [^interval].
	self searchStrings do: 
			[:each | 
			| search index |
			search := each isSymbol ifTrue: [each keywords first] ifFalse: [each].
			index := aString indexOfSubCollection: search startingAt: 1.
			index > 0 ifTrue: [^index to: index + search size - 1]].
	^nil
]

{ #category : #accessing }
RBBrowserEnvironment >> selectionParseTreeIntervalFor: aString [ 
	| parseTree answerBlock |
	parseTree := RBParser
		parseMethod: aString
		onError: [ :str :pos | ^ nil ].
	answerBlock := [ :aNode :answer | ^ aNode sourceInterval ].
	self searchStrings do: [ :each | 
		| matcher tree |
		matcher := RBParseTreeSearcher new.
		matcher matchesTree: (RBLiteralNode value: each) do: answerBlock.
		each isSymbol 
			ifTrue: [
				tree := RBParseTreeSearcher buildSelectorTree: each.
				tree ifNotNil: [ matcher matchesTree: tree do: answerBlock ] ]
			ifFalse: [
				tree := RBVariableNode named: each.
				matcher
					matchesTree: tree do: answerBlock;
					matchesArgumentTree: tree do: answerBlock ].
		matcher executeTree: parseTree ].
	^ nil
]

{ #category : #accessing }
RBBrowserEnvironment >> selectors [
	^ Array streamContents: [ :stream |
		self selectorsDo: [ :selector | 
			stream nextPut: selector ]]
]

{ #category : #accessing }
RBBrowserEnvironment >> selectorsDo: aBlock [
	self allClassesDo: [ :each | 
		self selectorsForClass: each do: aBlock ].
]

{ #category : #accessing }
RBBrowserEnvironment >> selectorsFor: aProtocol in: aClass [
	^(aClass organization listAtCategoryNamed: aProtocol)
		select: [:each | self includesSelector: each in: aClass]
]

{ #category : #accessing }
RBBrowserEnvironment >> selectorsForClass: aClass [ 
	| selectors |
	selectors := IdentitySet new.
	self selectorsForClass: aClass do: [ :each | selectors add: each ].
	^ selectors
]

{ #category : #accessing }
RBBrowserEnvironment >> selectorsForClass: aClass do: aBlock [ 
	aClass selectorsAndMethodsDo: 
			[:each :meth | 
			(self includesSelector: each in: aClass) ifTrue: [aBlock value: each]]
]

{ #category : #printing }
RBBrowserEnvironment >> storeOn: aStream [ 
	aStream
		nextPutAll: self class name;
		nextPutAll: ' new'
]

{ #category : #private }
RBBrowserEnvironment >> systemDictionary [
	"The root system dictionary as the source of all objects in this environment."

	^ Smalltalk globals
]

{ #category : #private }
RBBrowserEnvironment >> systemDictionaryClassesDo: aBlock [
	self systemDictionary allClassesDo:[:class |
		 aBlock value: class; value: class classSide]
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> traits [
	| traits |
	traits := IdentitySet new: 4096.
	self traitsDo: [ :each | traits add: each ].
	^ traits
]

{ #category : #'accessing-classes' }
RBBrowserEnvironment >> traitsDo: aBlock [

	self systemDictionary allTraitsDo: [ :each | 
		(self includesClass: each)
			ifTrue: [ aBlock value: each ]  ]
]

{ #category : #accessing }
RBBrowserEnvironment >> whichCategoryIncludes: aClassName [
	^ self systemDictionary organization categoryOfElement: aClassName
]

{ #category : #accessing }
RBBrowserEnvironment >> whichProtocolIncludes: aSelector in: aClass [
	^aClass organization categoryOfElement: aSelector ifAbsent: [ Protocol unclassified ]
]

{ #category : #environments }
RBBrowserEnvironment >> withoutClasses: aListOfClasses [
	"Build the same environment without classes given as parameter"
	^ RBClassEnvironment 
			onEnvironment: self 
			classes: (self classes copyWithoutAll: aListOfClasses)
]

{ #category : #environments }
RBBrowserEnvironment >> | anEnvironment [ 
	"If we or anEnvironment includes everything, then return it instead of creating 
	an or that will include everything."

	self isSystem ifTrue: [^self].
	anEnvironment isSystem ifTrue: [^anEnvironment].
	^ RBOrEnvironment onEnvironment: self or: anEnvironment
]
