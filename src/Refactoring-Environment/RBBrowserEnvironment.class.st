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
	#name : 'RBBrowserEnvironment',
	#superclass : 'Object',
	#instVars : [
		'label',
		'searchStrings',
		'accessGuard',
		'queryCache',
		'plugins',
		'updateStrategy',
		'updateGuard'
	],
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'accessing' }
RBBrowserEnvironment class >> default [
	^ RBBrowserEnvironment new
]

{ #category : 'accessing' }
RBBrowserEnvironment class >> defaultName [
	^ 'Current image'
]

{ #category : 'environments' }
RBBrowserEnvironment >> & anEnvironment [
	"If we or anEnvironment includes everything, then just include the other environment (optimization)"

	self isSystem ifTrue: [^anEnvironment].
	anEnvironment isSystem ifTrue: [^self].
	^RBAndEnvironment onEnvironment: self and: anEnvironment
]

{ #category : 'accessing' }
RBBrowserEnvironment >> accessGuard [
	^ accessGuard ifNil: [ accessGuard := Mutex new ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> addSearchString: aString [

	searchStrings
		ifNil: [ searchStrings := SortedCollection
				sortBlock: [ :a :b | ( a indexOf: $: ifAbsent: [ a size ] ) > ( b indexOf: $: ifAbsent: [ b size ] ) ]
			].
	( searchStrings includes: aString )
		ifFalse: [ searchStrings add: aString ]
]

{ #category : 'queries' }
RBBrowserEnvironment >> adoptQuery: aQuery [
	aQuery scope: self.
	^aQuery
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> allClasses [
	"Answer all the unique non-metaclasses of all the classes and metaclasses in this environment."

	| allClasses |
	allClasses := IdentitySet new: 4096.
	self classesDo: [ :each |
		allClasses add: each instanceSide].
	^ allClasses asArray
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> allClassesAndTraits [
	" compatibility method with SystemEnvironment"

	| allClassesAndTraits |
	allClassesAndTraits := OrderedCollection new: 4096.
	self allClassesAndTraitsDo: [ :each |
		allClassesAndTraits add: each ].
	^ allClassesAndTraits asArray
]

{ #category : 'private' }
RBBrowserEnvironment >> allClassesAndTraitsDo: aBlock [
	self systemDictionary allClassesAndTraitsDo: aBlock
]

{ #category : 'private' }
RBBrowserEnvironment >> allClassesDo: aBlock [
	self systemDictionaryClassesDo: aBlock
]

{ #category : 'accessing - classes' }
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

{ #category : 'accessing - classes' }
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

{ #category : 'announcements' }
RBBrowserEnvironment >> announceChangesOf: aQueryResult [

	self updateStrategy announceChangesOf: aQueryResult
]

{ #category : 'converting' }
RBBrowserEnvironment >> asRBEnvironment [

	^ self
]

{ #category : 'accessing' }
RBBrowserEnvironment >> asSelectorEnvironment [
	^(RBClassEnvironment onEnvironment: self classes: self classes) asSelectorEnvironment
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> associationAt: aKey [
	^ self associationAt: aKey ifAbsent: [ self error: aKey printString , ' not found' ]
]

{ #category : 'accessing - classes' }
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

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> at: aKey [
	^ self at: aKey ifAbsent: [ self error: aKey printString , ' not found' ]
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> at: aKey ifAbsent: aBlock [

	| association |

	association := self associationAt: aKey ifAbsent: [ nil ].
	^ association ifNil: [ aBlock value ] ifNotNil: [ association value ]
]

{ #category : 'queries' }
RBBrowserEnvironment >> cachedResultOf: aQuery [

	^self queryCache at: aQuery ifAbsent: [ nil ]
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> classNames [
	| names |
	names := IdentitySet new: 4096.
	self classesDo: [ :each | names add: each instanceSide name ].
	^ names
]

{ #category : 'environments' }
RBBrowserEnvironment >> classVarRefsTo: instVarName in: aClass [
	^ RBVariableEnvironment
		on: self
		referencesToClassVariable: instVarName
		in: aClass
]

{ #category : 'accessing' }
RBBrowserEnvironment >> classVariablesFor: aClass [
	^aClass classVarNames
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> classes [
	| classes |
	classes := IdentitySet new: 4096.
	self classesDo: [ :each | classes add: each ].
	^ classes
]

{ #category : 'accessing' }
RBBrowserEnvironment >> classesAndSelectorsDo: aBlock [
	self classesDo: [ :class |
		self selectorsForClass: class do: [ :sel |
			aBlock value: class value: sel ]]
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> classesAndTraits [

	| classesAndTraits |
	classesAndTraits := OrderedCollection new: 4096.
	self classesAndTraitsDo: [ :each |
		classesAndTraits add: each ].
	^ classesAndTraits asArray
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> classesAndTraitsDo: aBlock [
	self allClassesAndTraitsDo: [ :each |
		(self includesClass: each)
			ifTrue: [ aBlock value: each ] ]
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> classesDo: aBlock [
	self systemDictionaryClassesDo: [ :each |
		(self includesClass: each)
			ifTrue: [aBlock value: each ] ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> classesInPackage: aPackage [

	^ aPackage definedClasses select: [ :class | (self includesClass: class) or: [ self includesClass: class class ] ]
]

{ #category : 'cleaning' }
RBBrowserEnvironment >> cleanGarbageInCache [

	self accessGuard critical: [ self queryCache clyCleanGarbage ]
]

{ #category : 'copying' }
RBBrowserEnvironment >> copyEmpty [
	^ self class new
]

{ #category : 'private' }
RBBrowserEnvironment >> defaultLabel [
	^ 'Smalltalk'
]

{ #category : 'accessing' }
RBBrowserEnvironment >> defaultName [

	^ self class defaultName
]

{ #category : 'accessing' }
RBBrowserEnvironment >> defaultPlugins [

	^ OrderedCollection new
]

{ #category : 'testing' }
RBBrowserEnvironment >> definesClass: aClass [
	^ true
]

{ #category : 'description' }
RBBrowserEnvironment >> description [

	^ self defaultName
]

{ #category : 'description' }
RBBrowserEnvironment >> descriptionUntil: aNumber [

	^ self description contractTo: aNumber
]

{ #category : 'private' }
RBBrowserEnvironment >> environment [
	^ self
]

{ #category : 'environments' }
RBBrowserEnvironment >> forClass: aClass protocols: protocolCollection [
	^RBProtocolEnvironment onEnvironment: self
		class: aClass
		protocols: protocolCollection
]

{ #category : 'environments' }
RBBrowserEnvironment >> forClass: aClass selectors: selectorCollection [
	^(RBSelectorEnvironment
		onMethods: selectorCollection
		forClass: aClass
		in: self)
		label: aClass name , '>>'
					, (selectorCollection detect: [:each | true] ifNone: ['']);
		yourself
]

{ #category : 'environments' }
RBBrowserEnvironment >> forClassHierarchy: aClass [
	^ RBClassHierarchyEnvironment class: aClass
]

{ #category : 'environments' }
RBBrowserEnvironment >> forClasses: classCollection [
	| classes |
	classes := OrderedCollection new: classCollection size * 2.
	classCollection do:
			[:each |
			classes add: each instanceSide; add: each classSide].
	^RBClassEnvironment onEnvironment: self classes: classes
]

{ #category : 'environments' }
RBBrowserEnvironment >> forPackageNames: aCollection [
	^ RBPackageEnvironment onEnvironment: self packageNames: aCollection
]

{ #category : 'environments' }
RBBrowserEnvironment >> forPackages: aCollection [
	^ RBPackageEnvironment onEnvironment: self packages: aCollection
]

{ #category : 'environments' }
RBBrowserEnvironment >> forPragmas: aKeywordCollection [
	^ RBPragmaEnvironment onEnvironment: self keywords: aKeywordCollection
]

{ #category : 'announcements' }
RBBrowserEnvironment >> handleSystemChange: aSystemAnnouncement [

	| todoList todoSize anyResult |
	todoList := 	(queryCache values select: [ :each | each isNotNil ]) as: IdentitySet.
	todoSize := 0.
	[[todoSize = todoList size] whileFalse: [
		todoSize := todoList size.
		todoList asArray do: [ :eachResult |
			(todoList includes: eachResult) ifTrue: [
				eachResult handleSystemChange: aSystemAnnouncement byProcessingList: todoList]]].
	todoList notEmpty] whileTrue: [
		anyResult := todoList anyOne.
		anyResult handleSystemChange: aSystemAnnouncement.
		todoList remove: anyResult]
]

{ #category : 'environments' }
RBBrowserEnvironment >> implementorsMatching: aString [
	^RBSelectorEnvironment implementorsMatching: aString in: self
]

{ #category : 'environments' }
RBBrowserEnvironment >> implementorsOf: aSelector [
	^RBSelectorEnvironment implementorsOf: aSelector in: self
]

{ #category : 'testing' }
RBBrowserEnvironment >> includesClass: aClass [
	^ true
]

{ #category : 'testing' }
RBBrowserEnvironment >> includesMethod: aMethod [
	^ self
		includesSelector: aMethod selector
		in: aMethod methodClass
]

{ #category : 'testing' }
RBBrowserEnvironment >> includesPackage: packageName [
	^ true
]

{ #category : 'testing' }
RBBrowserEnvironment >> includesProtocol: aProtocol in: aClass [
	^ true
]

{ #category : 'testing' }
RBBrowserEnvironment >> includesSelector: aSelector in: aClass [
	^ true
]

{ #category : 'environments' }
RBBrowserEnvironment >> instVarReadersTo: instVarName in: aClass [
	^RBVariableEnvironment
		on: self
		readersOfInstanceVariable: instVarName
		in: aClass
]

{ #category : 'environments' }
RBBrowserEnvironment >> instVarRefsTo: instVarName in: aClass [
	^ RBVariableEnvironment
		on: self
		referencesToInstanceVariable: instVarName
		in: aClass
]

{ #category : 'environments' }
RBBrowserEnvironment >> instVarWritersTo: instVarName in: aClass [
	^RBVariableEnvironment
		on: self
		writersOfInstanceVariable: instVarName
		in: aClass
]

{ #category : 'accessing' }
RBBrowserEnvironment >> instanceVariablesFor: aClass [
	^aClass instVarNames
]

{ #category : 'testing' }
RBBrowserEnvironment >> isBasedOnEmptyBasis [

	^false
]

{ #category : 'testing' }
RBBrowserEnvironment >> isBoundToEnvironment [

	^ true
]

{ #category : 'testing' }
RBBrowserEnvironment >> isClassEnvironment [
	^ false
]

{ #category : 'testing' }
RBBrowserEnvironment >> isCompositeEnvironment [

	^ false
]

{ #category : 'testing' }
RBBrowserEnvironment >> isEmpty [
	^ false
]

{ #category : 'testing' }
RBBrowserEnvironment >> isSelectorEnvironment [
	^ false
]

{ #category : 'testing' }
RBBrowserEnvironment >> isSystem [
	^ true
]

{ #category : 'testing' }
RBBrowserEnvironment >> isVariableEnvironment [
	^ false
]

{ #category : 'accessing - classes' }
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

{ #category : 'private' }
RBBrowserEnvironment >> label [

	^ label ifNil: [ self defaultLabel ] ifNotNil: [ label ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> label: aString [
	label := aString
]

{ #category : 'environments' }
RBBrowserEnvironment >> matches: aString [
	^RBSelectorEnvironment matches: aString in: self
]

{ #category : 'accessing' }
RBBrowserEnvironment >> methods [
	| methods |
	methods := IdentitySet new: 4096.
	self methodsDo: [ :each | methods add: each ].
	^ methods
]

{ #category : 'accessing' }
RBBrowserEnvironment >> methodsDo: aBlock [

	self classesDo: [ :aClass |
		self methodsForClass: aClass do: [ :method | aBlock value: method ].
		self methodsForClass: aClass class do: [ :method | aBlock value: method ] ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> methodsForClass: aClass do: aBlock [

	aClass selectorsAndMethodsDo: [ :each :meth | (self includesSelector: each in: aClass) ifTrue: [ aBlock value: meth ] ]
]

{ #category : 'environments' }
RBBrowserEnvironment >> not [
	self isSystem ifTrue: [^RBSelectorEnvironment new].
	^RBNotEnvironment onEnvironment: self
]

{ #category : 'accessing' }
RBBrowserEnvironment >> numberClasses [
	^self classNames size
]

{ #category : 'accessing' }
RBBrowserEnvironment >> numberSelectors [
	| total |
	total := 0.
	self selectorsDo: [:sel | total := total + 1 ].
	^total
]

{ #category : 'accessing - packages' }
RBBrowserEnvironment >> packageAt: aName [
	^ self packageAt: aName ifAbsent: [ NotFound signalFor: aName in: self ]
]

{ #category : 'accessing - packages' }
RBBrowserEnvironment >> packageAt: aName ifAbsent: absentBlock [

	^ self packages
		  detect: [ :package | package name sameAs: aName ]
		  ifNone: absentBlock
]

{ #category : 'accessing - packages' }
RBBrowserEnvironment >> packages [
	"Contrary to `full packages`, `packages` include both packages directly defined in the environment and packages from the clases belonging to the environment. For example, if an environment contains the class Object, the package Kernel is a defined package in the environment, even though this package does is not directly defined in the environment "
	^ self packageOrganizer packages select: [ :package | self includesPackage: package ]
]

{ #category : 'accessing - packages' }
RBBrowserEnvironment >> packagesWithContributors [
	"answer a list of all packages contributing to the definition of classes in the environment"
	^ self classes
		flatCollect: #packages
		as: Set
]

{ #category : 'accessing' }
RBBrowserEnvironment >> plugins [

	^ plugins ifNil: [ self defaultPlugins ]
]

{ #category : 'actions' }
RBBrowserEnvironment >> pluginsDo: aBlock [
	^ self plugins do: aBlock
]

{ #category : 'printing' }
RBBrowserEnvironment >> printDescriptionOn: aStream [

	aStream nextPutAll: self description
]

{ #category : 'printing' }
RBBrowserEnvironment >> printOn: aStream [
	aStream nextPutAll: self label
]

{ #category : 'accessing' }
RBBrowserEnvironment >> problemCount [
	^self numberSelectors
]

{ #category : 'accessing' }
RBBrowserEnvironment >> protocolsFor: aClass [

	^ aClass protocols select: [ :protocol | self includesProtocol: protocol in: aClass ]
]

{ #category : 'queries' }
RBBrowserEnvironment >> query: aQuery [

	| result |
	self cleanGarbageInCache.
	result := self queryCache at: aQuery ifAbsent: [nil]. "cache is weak dict where ifAbsentPut: not works"
	result ifNil: [
		result := aQuery prepareNewResult.
		aQuery fixStateBeforeExecution.
		"We should ensure that state of query will not be modified after execution
		because it is the key in cache.
		So aQuery is supposed to become readonly object together with required internal state"
		self accessGuard critical: [ self queryCache at: aQuery put: result]].
	result rebuildIfNeeded.
	^result
]

{ #category : 'accessing' }
RBBrowserEnvironment >> queryCache [

	^queryCache ifNil: [ queryCache := WeakValueDictionary new ]
]

{ #category : 'environments' }
RBBrowserEnvironment >> referencesTo: aLiteral [
	^RBSelectorEnvironment referencesTo: aLiteral in: self
]

{ #category : 'environments' }
RBBrowserEnvironment >> referencesTo: aLiteral in: aClass [
	| classes |
	classes := aClass withAllSuperclasses asSet.
	classes
		addAll: aClass allSubclasses;
		addAll: aClass class withAllSuperclasses;
		addAll: aClass class allSubclasses.
	^(self forClasses: classes) referencesTo: aLiteral
]

{ #category : 'private' }
RBBrowserEnvironment >> rootEnvironment [
	"The root environment representing everything."

	^ self
]

{ #category : 'accessing' }
RBBrowserEnvironment >> searchStrings [

	^ searchStrings ifNil: [ #() ] ifNotNil: [ searchStrings ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> searchStrings: aCollection [
	searchStrings := aCollection
]

{ #category : 'environments' }
RBBrowserEnvironment >> selectMethods: aBlock [
	| env |
	env := RBSelectorEnvironment onEnvironment: self.
	self classesAndSelectorsDo:
			[:each :sel |
			(aBlock value: (each compiledMethodAt: sel))
				ifTrue: [env addClass: each selector: sel]].
	^env
]

{ #category : 'accessing' }
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

{ #category : 'accessing' }
RBBrowserEnvironment >> selectionParseTreeIntervalFor: aString [
	| parseTree answerBlock |
	parseTree := RBParser
		parseMethod: aString
		onError: [ :str :pos | ^ nil ].
	answerBlock := [ :aNode :answer | ^ aNode sourceInterval ].
	self searchStrings do: [ :each |
		| matcher tree |
		matcher := RBParseTreeSearcher new.
		matcher matchesTree: (ASTLiteralNode value: each) do: answerBlock.
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

{ #category : 'accessing' }
RBBrowserEnvironment >> selectors [
	^ Array streamContents: [ :stream |
		self selectorsDo: [ :selector |
			stream nextPut: selector ]]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> selectorsDo: aBlock [
	self allClassesDo: [ :each |
		self selectorsForClass: each do: aBlock ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> selectorsFor: aProtocol in: aClass [

	^ (aClass selectorsInProtocol: aProtocol) select: [ :selector | self includesSelector: selector in: aClass ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> selectorsForClass: aClass [
	| selectors |
	selectors := IdentitySet new.
	self selectorsForClass: aClass do: [ :each | selectors add: each ].
	^ selectors
]

{ #category : 'accessing' }
RBBrowserEnvironment >> selectorsForClass: aClass do: aBlock [
	aClass selectorsAndMethodsDo:
			[:each :meth |
			(self includesSelector: each in: aClass) ifTrue: [aBlock value: each]]
]

{ #category : 'description' }
RBBrowserEnvironment >> shortDescription [

	^ self description contractTo: 50
]

{ #category : 'storing' }
RBBrowserEnvironment >> storeOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: ' new'
]

{ #category : 'testing' }
RBBrowserEnvironment >> supportsQuery: aQuery [

	^true
]

{ #category : 'private' }
RBBrowserEnvironment >> systemDictionary [
	"The root system dictionary as the source of all objects in this environment."

	^ Smalltalk globals
]

{ #category : 'private' }
RBBrowserEnvironment >> systemDictionaryClassesDo: aBlock [
	self systemDictionary allClassesDo:[:class |
		 aBlock value: class; value: class classSide]
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> traits [
	| traits |
	traits := IdentitySet new: 4096.
	self traitsDo: [ :each | traits add: each ].
	^ traits
]

{ #category : 'accessing - classes' }
RBBrowserEnvironment >> traitsDo: aBlock [

	self systemDictionary allTraitsDo: [ :each |
		(self includesClass: each)
			ifTrue: [ aBlock value: each ]  ]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> updateGuard [

	^ updateGuard ifNil: [ updateGuard := Mutex new ]
]

{ #category : 'actions' }
RBBrowserEnvironment >> updateUsing: newUpdateStrategy by: updateBlock [
	"here is special logic to break current mutex when any error is signalled.
	Without this logic fixing code inside spawned debugger will lead to deadlock
	because applying method changes will blocked at this accessGuard.
	Breaking mutex is done by creating new one which meand that current process
	is not guarded any more and proceeding execution in debugger can lead to some errors in rare cases. But it is less problem than locked UI"
	[
		self updateGuard critical: [ | oldStrategy |
			oldStrategy := updateStrategy.
			[
				updateStrategy := newUpdateStrategy.
				updateBlock on: Error do: [ :err |
					updateStrategy := oldStrategy.
					updateGuard := Mutex new.
					err pass]
			] ensure: [
				self updateStrategy == newUpdateStrategy ifTrue: [
					"In case of error another process can set up new strategy.
					This condition will avoid possible collision"
					updateStrategy := oldStrategy ]]]
	] ensure: [ newUpdateStrategy publishCollectedChanges]
]

{ #category : 'accessing' }
RBBrowserEnvironment >> whichProtocolIncludes: aSelector in: aClass [
	"CyrilFerlicot: We should check if it is not better to return a protocol instead of a protocol name here and adapt the clients."

	^ (aClass protocolOfSelector: aSelector)
		  ifNotNil: [ :protocol | protocol name ]
		  ifNil: [ Protocol unclassified ]
]

{ #category : 'environments' }
RBBrowserEnvironment >> withoutClasses: aListOfClasses [
	"Build the same environment without classes given as parameter"
	^ RBClassEnvironment
			onEnvironment: self
			classes: (self classes copyWithoutAll: aListOfClasses)
]

{ #category : 'environments' }
RBBrowserEnvironment >> | anEnvironment [
	"If we or anEnvironment includes everything, then return it instead of creating
	an or that will include everything."

	self isSystem ifTrue: [^self].
	anEnvironment isSystem ifTrue: [^anEnvironment].
	^ RBOrEnvironment onEnvironment: self or: anEnvironment
]
