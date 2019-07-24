"
i'm a cache to improve loading of critics
"
Class {
	#name : #CBCriticsCache,
	#superclass : #Object,
	#instVars : [
		'critics',
		'toDos',
		'falsePositives',
		'checker',
		'browser',
		'falsePositiveRules',
		'falsePositiveClasses',
		'packages',
		'change'
	],
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #private }
CBCriticsCache >> addAllCriticToFalsePositive: aCollectionOfCritic forRule: aRule on: aManifestBuilder [

	| ruleId versionId  criticss |
	criticss := aCollectionOfCritic 
						reject: [ :c | falsePositiveClasses includes: c criticTheNonMetaclassClass ].
	ruleId := aRule class uniqueIdentifierName.
	versionId := aRule class identifierMinorVersionNumber.
	(aManifestBuilder hasFalsePositiveOf: ruleId version: versionId) 
		ifFalse: [ aManifestBuilder installFalsePositiveOf: ruleId version: versionId ].		
	 aManifestBuilder addAllFalsePositive: criticss of: ruleId version: versionId

	
]

{ #category : #private }
CBCriticsCache >> addAllCriticToToDo: aCollectionOfCritic forRule: aRule on: aManifestBuilder [

	| ruleId versionId  |
	ruleId := aRule class uniqueIdentifierName.
	versionId := aRule class identifierMinorVersionNumber.
	(aManifestBuilder hasToDoOf: ruleId version: versionId) 
		ifFalse: [ aManifestBuilder installToDoOf: ruleId version: versionId ].		
	 aManifestBuilder addAllToDo: aCollectionOfCritic of: ruleId version: versionId

	
]

{ #category : #'add/remove' }
CBCriticsCache >> addCritic: aCritic forRule: aRule [

	(critics includesKey: aRule) 
		ifFalse: [ critics at:aRule put: IdentitySet new ].
	(critics at: aRule) add: aCritic. 
	self updateBrowser
]

{ #category : #'add/remove' }
CBCriticsCache >> addFalsePositive: aCritic forRule: aRule [
	(falsePositives includesKey: aRule) 
		ifFalse: [ falsePositives at: aRule put: IdentitySet new ].
	(falsePositives at: aRule) add: aCritic. 
	self updateBrowser 	
]

{ #category : #'add/remove' }
CBCriticsCache >> addFalsePositiveClass: aClass [
	falsePositiveClasses add: aClass instanceSide.
	critics keysAndValuesDo: [ :rule :criticss | criticss select: [ :critic | aClass = critic criticTheNonMetaclassClass ] thenDo: [ :each | self addFalsePositive: each forRule: rule ] ].
	self updateBrowser
]

{ #category : #'add/remove' }
CBCriticsCache >> addFalsePositiveRule: aRule forPackage: aPackage [
	| fp |
	(falsePositiveRules includesKey: aPackage)
		ifFalse: [ falsePositiveRules at: aPackage put: Set new ].
	(falsePositiveRules at: aPackage)
		add: aRule class uniqueIdentifierName.
	fp := (critics at: aRule ifAbsent: [ ^ self ])
		select:
			[ :aCritic | (self packageOf: aCritic) package name = aPackage packageName ].
	fp do: [ :aCritic | self addFalsePositive: aCritic forRule: aRule ]
]

{ #category : #'add/remove' }
CBCriticsCache >> addToDo: aCritic forRule: aRule [

	(toDos includesKey: aRule) 
		ifFalse: [ toDos at:aRule put: IdentitySet new ].
	(toDos at: aRule) add: aCritic. 
	self updateBrowser
]

{ #category : #accessing }
CBCriticsCache >> browser: aCodeCritiicBrowser [
	browser := aCodeCritiicBrowser 
]

{ #category : #private }
CBCriticsCache >> builderManifestClass [
	^ TheManifestBuilder
]

{ #category : #accessing }
CBCriticsCache >> cacheChange [
	^ change
]

{ #category : #private }
CBCriticsCache >> cacheChanged [ 
	change := true
]

{ #category : #private }
CBCriticsCache >> cacheNotChanged [ 
	change := false
]

{ #category : #accessing }
CBCriticsCache >> checker [
	^ checker
]

{ #category : #accessing }
CBCriticsCache >> checker: aSmallLintChercker [
	checker := aSmallLintChercker
]

{ #category : #accessing }
CBCriticsCache >> critics [
	^ critics
]

{ #category : #protocol }
CBCriticsCache >> criticsOf: aRule [

	aRule ifNil: [ ^ {} ].	
	^ aRule isComposite
		ifTrue: [ aRule rules flatCollect: [ :rule | self criticsOf: rule ]]
		ifFalse: [ critics at: aRule ifAbsent: [ {} ]]
]

{ #category : #accessing }
CBCriticsCache >> falsePositiveClass [
	^ falsePositiveClasses
]

{ #category : #protocol }
CBCriticsCache >> falsePositiveOf: aRule [
	
	aRule ifNil: [ ^ {}].
	^ aRule isComposite
		ifTrue: [ aRule rules flatCollect: [ :rule | self falsePositiveOf: rule ]]
		ifFalse: [ falsePositives at: aRule ifAbsent: [ {} ]]
	
	
]

{ #category : #accessing }
CBCriticsCache >> falsePositiveRules [
	^ falsePositiveRules
]

{ #category : #accessing }
CBCriticsCache >> falsePositives [
	^ falsePositives
]

{ #category : #initialization }
CBCriticsCache >> initCache [
	checker rule
		do: [ :rule | 
			falsePositives
				at: rule
				put: (IdentitySet newFrom: (checker falsePositiveOf: rule)).
			toDos at: rule put: (IdentitySet newFrom: (checker toDoOf: rule)).
			critics
				at: rule
				put: (IdentitySet newFrom: (checker criticsOf: rule)) ].
	packages
		do: [ :package | 
			falsePositiveRules
				at: package
				put: (checker rejectRulesOf: package) asSet.
			falsePositiveClasses
				addAll: (checker rejectClassesOf: package) asIdentitySet ]
]

{ #category : #initialization }
CBCriticsCache >> initialize [
	
	super initialize.
	falsePositives := Dictionary new.
	toDos := Dictionary new.	
	critics := Dictionary new.	
	falsePositiveRules := Dictionary new.
	falsePositiveClasses := IdentitySet new.
	change := false	

]

{ #category : #testing }
CBCriticsCache >> isFalsePositive: aCritic forRule: aRule [
	^ (self falsePositiveOf: aRule) includes: aCritic
]

{ #category : #testing }
CBCriticsCache >> isToDo: aCritic forRule: aRule [
	^ (self toDosOf: aRule) includes: aCritic
]

{ #category : #'add/remove' }
CBCriticsCache >> itemRemoved: anItem [
	"anItem : can be a method a class (where is the critics)"
	{ critics . toDos . falsePositives } do: [ :ruleDict |
		ruleDict valuesDo: [ :crits |
			crits removeAllSuchThat: [ :crit |
				crit sourceAnchor entity = anItem ]]]
	
]

{ #category : #private }
CBCriticsCache >> logFalsePositiveClassInManifest [
	| manifestBuilder |
	packages
		do: [ :package | 
			manifestBuilder := self builderManifestClass ofPackageNamed: package packageName.
			manifestBuilder rejectClasses \ falsePositiveClasses do: [ :cl | manifestBuilder removeRejectClass: cl ].
			falsePositiveClasses \ manifestBuilder rejectClasses do: [ :cl | manifestBuilder addRejectClass: cl ] ]
]

{ #category : #private }
CBCriticsCache >> logFalsePositiveInManifest [
	| removeFp addFp |
	falsePositives
		keysAndValuesDo: [ :rule :criticss | 
			removeFp := (checker falsePositiveOf: rule) \ criticss.
			addFp := criticss \ (checker falsePositiveOf: rule).
			(removeFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | self removeAllCriticToFalsePositive: value forRule: rule on: manifestBuilder ].
			(addFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | 
					(manifestBuilder rejectRules includes: rule class uniqueIdentifierName)
						ifFalse: [ self addAllCriticToFalsePositive: value forRule: rule on: manifestBuilder ] ] ]
]

{ #category : #private }
CBCriticsCache >> logFalsePositiveRulesInManifest [
	| manifestBuilder |
	falsePositiveRules
		keysAndValuesDo: [:package :rules | 
			manifestBuilder := self builderManifestClass ofPackageNamed: package packageName.
			manifestBuilder rejectRules \ rules do: [ :rule | manifestBuilder removeRejectRule: rule ].
			rules \ manifestBuilder rejectRules do: [ :rule | manifestBuilder addRejectRule: rule ]]
]

{ #category : #protocol }
CBCriticsCache >> logInManifest [

	self cacheChange 
		ifFalse: [^ self].
	self logFalsePositiveRulesInManifest.
	self logFalsePositiveClassInManifest.
	self logFalsePositiveInManifest.
	self logToDosInManifest.
	self cacheNotChanged. 		
	self initCache
	
]

{ #category : #private }
CBCriticsCache >> logToDosInManifest [
	| removeFp addFp |
	toDos
		keysAndValuesDo: [ :rule :criticss | 
			removeFp := (checker toDoOf: rule) \ criticss.
			addFp := criticss \ (checker toDoOf: rule).
			(removeFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | self removeAllCriticToToDo: value forRule: rule on: manifestBuilder ].
			(addFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | 
					(manifestBuilder rejectRules includes: rule class uniqueIdentifierName)
						ifFalse: [ self addAllCriticToToDo: value forRule: rule on: manifestBuilder ] ] ]
]

{ #category : #private }
CBCriticsCache >> packageOf: aCritc [
	^ aCritc sourceAnchor entity package
]

{ #category : #accessing }
CBCriticsCache >> packages: aPackageEnv [
	
	packages := aPackageEnv packages
]

{ #category : #private }
CBCriticsCache >> removeAllCriticToFalsePositive: aCollectionOfCritic forRule: aRule on: aManifestBuilder [

	| ruleId versionId |
	ruleId := aRule class uniqueIdentifierName.
	versionId := aRule class identifierMinorVersionNumber.
	(aManifestBuilder hasFalsePositiveOf: ruleId version: versionId) 
		ifTrue: [ aManifestBuilder removeAllFalsePositive: aCollectionOfCritic of: ruleId version: versionId ]	
	 

	
]

{ #category : #private }
CBCriticsCache >> removeAllCriticToToDo: aCollectionOfCritic forRule: aRule on: aManifestBuilder [

	| ruleId versionId |
	ruleId := aRule class uniqueIdentifierName.
	versionId := aRule class identifierMinorVersionNumber.
	(aManifestBuilder hasToDoOf: ruleId version: versionId) 
		ifTrue: [ aManifestBuilder removeAllToDo: aCollectionOfCritic of: ruleId version: versionId ]		
	

	
]

{ #category : #'add/remove' }
CBCriticsCache >> removeCritic: aCritic forRule: aRule [

	(critics includesKey: aRule) 
		ifFalse: [^ self].
	(critics at: aRule) remove: aCritic ifAbsent: [^ self].
	self updateBrowser
	
]

{ #category : #'add/remove' }
CBCriticsCache >> removeFalsePositive: aCritic forRule: aRule [
	(falsePositives includesKey: aRule) 
		ifFalse: [^ self ].
	[ (falsePositiveClasses includes: aCritic sourceAnchor entity criticTheNonMetaclassClass) 
			ifTrue: [^ self] ]
		on: MessageNotUnderstood 
		do: [ ^ self "#criticTheNonMetaclassClass makes no sense on a Package, ignore"].
	(falsePositives at: aRule) remove: aCritic ifAbsent: [^ self].
	self updateBrowser
]

{ #category : #'add/remove' }
CBCriticsCache >> removeFalsePositiveClass: aClass [
	
	| fp |
	falsePositiveClasses remove: aClass ifAbsent: [^ self]. 
	critics keysAndValuesDo: [ :rule :criticss |
		fp := criticss select: [ :critic | aClass = critic criticTheNonMetaclassClass].
		fp do: [ :each | self removeFalsePositive: each forRule: rule ]]
]

{ #category : #'add/remove' }
CBCriticsCache >> removeFalsePositiveRule: aRule forPackage: aPackage [
	| fp |
	(falsePositiveRules includesKey: aPackage)
		ifFalse: [ ^ self ].
	(falsePositiveRules at: aPackage)
		remove: aRule class uniqueIdentifierName
		ifAbsent: [ ^ self ].
	fp := (critics at: aRule ifAbsent: [ ^ self ])
		select: [ :c | 
			(self packageOf: c) package name = aPackage packageName
				and: [ (falsePositiveClasses
						includes: c sourceAnchor entity criticTheNonMetaclassClass) not ] ].
	fp do: [ :c | self removeFalsePositive: c forRule: aRule ].
	self updateBrowser
]

{ #category : #'add/remove' }
CBCriticsCache >> removeRule: aRule [

	{ critics . toDos . falsePositives } do: [ :ruleDict |
		ruleDict removeKey: aRule ]
]

{ #category : #'add/remove' }
CBCriticsCache >> removeToDo: aCritic forRule: aRule [
	(toDos includesKey: aRule)
		ifFalse: [ ^ self ].
	(toDos at: aRule) remove: aCritic ifAbsent: [ ^ self ].
	self updateBrowser
]

{ #category : #protocol }
CBCriticsCache >> replaceAll: oldMethod by: newMethod [

	critics valuesDo: [:each |  
		(each includes: oldMethod)
			ifTrue: [
				each remove: oldMethod.
				each add: newMethod ]].
		
	falsePositives valuesDo: [:each |  
		(each includes: oldMethod)
			ifTrue: [
				each remove: oldMethod.
				each add: newMethod ]].

	toDos valuesDo: [:each |  
		(each includes: oldMethod)
			ifTrue: [
				each remove: oldMethod.
				each add: newMethod ]]
	
]

{ #category : #accessing }
CBCriticsCache >> toDos [
	^ toDos
]

{ #category : #protocol }
CBCriticsCache >> toDosOf: aRule [

	aRule ifNil: [ ^{} ].
	^ aRule isComposite
		ifTrue: [ aRule rules flatCollect: [:rule | self toDosOf: rule ]]
		ifFalse: [toDos at: aRule ifAbsent: [ {} ]]
]

{ #category : #private }
CBCriticsCache >> updateBrowser [
	self cacheChanged.
	"We add a nil check because in the tests we do not want necessarily to have a browser"
	browser ifNotNil: #updateTree
]
