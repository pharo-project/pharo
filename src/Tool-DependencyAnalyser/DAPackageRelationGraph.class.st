"
I'm a package dependency graph. 
To compute dependencies of a set of packages you are interested in, you can use:
```language=Pharo
DADependencyChecker new buildPackageRelationGraphFor: 'Tool-Diff'.
```

"
Class {
	#name : #DAPackageRelationGraph,
	#superclass : #Object,
	#instVars : [
		'packages',
		'classMapping',
		'shouldComputeMessageSendDependencies'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #'instance creation' }
DAPackageRelationGraph class >> new [
	^ self onPackages: OrderedCollection new
]

{ #category : #'instance creation' }
DAPackageRelationGraph class >> onPackages: aListOfDAPackages [
	^ self basicNew
		initializeWithPackages: aListOfDAPackages;
		yourself
]

{ #category : #'instance creation' }
DAPackageRelationGraph class >> onPackagesNamed: packageNames [

	^ self onPackages: (packageNames collect: [ :each | DAPackage onPackageNamed: each ])
]

{ #category : #adding }
DAPackageRelationGraph >> addExtensionDependencies: aPDPackage [
	aPDPackage rPackageSet extensionMethods
		do: [ :method | | packageOfExtendedClass |
			packageOfExtendedClass := self packageForBehavior: method methodClass.
			self addPackage: packageOfExtendedClass.
			aPDPackage
				add:
					((DAExtensionDependency from: aPDPackage to: packageOfExtendedClass)
						theClass: method methodClass;
						selector: method selector asSymbol;
						method: method).
			 ]
]

{ #category : #adding }
DAPackageRelationGraph >> addInheritanceDependencies: aPackage [
	| package |
	self package: aPackage
		classesDo: [ :class |
			class superclass ifNotNil: [
					package := self packageForBehavior: class superclass.
					aPackage
						add:
							((DAInheritanceDependency from: aPackage to: package)
								theClass: class;
								superclass: class superclass).
					self addPackage: package ] ]
]

{ #category : #adding }
DAPackageRelationGraph >> addMessageSendDependencies: aPackage [
	| analyser |
	analyser := (DAMessageSendAnalyzer basicNew
		initializeWith: aPackage;
		yourself).

	analyser manuallyResolvedDependencies do:
		[ :packageName |
		(analyser missingMethodsImplementedIn: packageName)
			ifEmpty: [ DAPotentialOutDatedDependencyWarning signalDependency: packageName on: aPackage packageName ]
			ifNotEmpty: 	[ :implementedMethods |
				aPackage add: ((DAMessageSendDependency from: aPackage to: (self daPackageWith: packageName))
					implementedMethods: implementedMethods;
					yourself) ] ]
]

{ #category : #adding }
DAPackageRelationGraph >> addPackage: aPDPackage [
	aPDPackage beIncluded.
	packages
		at: aPDPackage packageName
		put: aPDPackage
]

{ #category : #adding }
DAPackageRelationGraph >> addPackages: aListOfDAPackages [

	aListOfDAPackages
		do: [ :package | self addPackage: package ]
]

{ #category : #adding }
DAPackageRelationGraph >> addPoolDictionaryDependencies: aPackage [
	"Iterate all classes in the package.
	Add a pool dictionary dependency for each pool dictionary used in the package."

	self package: aPackage
		classesDo: [ :class |
			class sharedPoolsDo: [ :each | | targetPackage |
				targetPackage := self packageForBehavior: each.
				aPackage
					add: ((DAPoolDictionaryDependency from: aPackage to: targetPackage)
						theClass: class;
						poolDictionary: each).
				self addPackage: targetPackage ] ]
]

{ #category : #adding }
DAPackageRelationGraph >> addReferenceDependencies: aPackage [
	(self methodsFor: aPackage)
		do: [ :method | self findReferencesIn: method for: aPackage ]
]

{ #category : #adding }
DAPackageRelationGraph >> addTraitDependencies: aPackage [
	| package |
	self package: aPackage
		classesDo: [ :class |
			class traits
				do: [ :trait |
					package := self packageForBehavior: trait.
					aPackage
						add:
							((DATraitDependency from: aPackage to: package)
								theClass: class;
								trait: trait).
					self addPackage: package ] ]
]

{ #category : #accessing }
DAPackageRelationGraph >> allDependentPackagesFromSeenPackages [
	| aSet |
	aSet := Set new.
	self seenPackages do: [ :seenPackage |
		aSet addAll: (seenPackage dependentPackages) ].

	^ aSet
]

{ #category : #computing }
DAPackageRelationGraph >> allExtensionDependencies [
	| aCollectionOfExtensionDependency |
	aCollectionOfExtensionDependency := OrderedCollection new.

	self seenPackages do: [ :each |
		each dependencies do: [ :dependency |
			aCollectionOfExtensionDependency addAll: dependency extensionDependencies.
			 ]
		 ].
	^ aCollectionOfExtensionDependency
]

{ #category : #computing }
DAPackageRelationGraph >> allReferenceDependencies [
	| aCollectionOfReferenceDependency |
	aCollectionOfReferenceDependency := OrderedCollection new.

	self seenPackages do: [ :each |
		each dependencies do: [ :dependency |
			aCollectionOfReferenceDependency addAll: dependency referenceDependencies.
			 ]
		 ].
	^ aCollectionOfReferenceDependency
]

{ #category : #testing }
DAPackageRelationGraph >> allSeen [

	^ self packages allSatisfy: [ :each | each isSeen ]
]

{ #category : #building }
DAPackageRelationGraph >> build [
	"build the dependency graph"
	self
		computeStaticDependencies;
		removeInternalDependencies;
		combineDependencies
]

{ #category : #adding }
DAPackageRelationGraph >> classesFor: aPackage [
	^ aPackage rPackageSet classes
]

{ #category : #actions }
DAPackageRelationGraph >> clearAllDependencies [
	packages do: [ :package | package clearDependencies ]
]

{ #category : #updating }
DAPackageRelationGraph >> clearPackages [
	packages := Dictionary new
]

{ #category : #actions }
DAPackageRelationGraph >> combineDependencies [
	"Combine all dependencies with the same source and target into one composite dependency."
	self packages do: [ :each | self combineDependencies: each ]
]

{ #category : #private }
DAPackageRelationGraph >> combineDependencies: aPackage [
	| grouped composite |
	grouped := aPackage dependencies
		groupedBy: [ :each | each target ]
		having: [ :each | true ].

	aPackage clearDependencies.

	grouped keysAndValuesDo: [ :target :dependencies |
		composite := DACompositeDependency from: aPackage to: target.
		dependencies do: [ :each | composite add: each ].
		aPackage add: composite ]
]

{ #category : #adding }
DAPackageRelationGraph >> computeStaticDependencies [
	self packages
		do: [ :each |
			each isSeen
				ifFalse: [
					self computeStaticDependencies: each.
					each beSeen ] ]
]

{ #category : #adding }
DAPackageRelationGraph >> computeStaticDependencies: aPackage [

	self addInheritanceDependencies: aPackage.
	self addExtensionDependencies: aPackage.
	self addReferenceDependencies: aPackage.
	self addPoolDictionaryDependencies: aPackage.
	self addTraitDependencies: aPackage.
	self shouldComputeMessageSendDependencies 
		ifTrue: [ self addMessageSendDependencies: aPackage ]
]

{ #category : #accessing }
DAPackageRelationGraph >> daPackageWith: packageName [
	^ DAPackage on: (RPackageSet named: packageName)
]

{ #category : #accessing }
DAPackageRelationGraph >> dependenciesFrom: aPackage [
	"Answer all seen packages."

	^ (packages at: aPackage) dependencies
]

{ #category : #adding }
DAPackageRelationGraph >> findReferencesIn: aMethod for: aPackage [
	| package compiledMethod allCompiledCode |

	compiledMethod := aMethod compiledMethod.
	compiledMethod ifNil: [ ^ self ].

	allCompiledCode := compiledMethod withAllBlocks.

	allCompiledCode do: [ :aCompiledCode |
			aCompiledCode literals do: [ :literal |
					(self isReference: literal)
						ifTrue: [
							package := self packageForBehavior: literal value.
							self addPackage: package.
							aPackage
								add:
									((DAReferenceDependency from: aPackage to: package)
										theClass: aMethod methodClass;
										selector: aMethod method selector asSymbol;
										reference: literal value;
										method: aMethod;
										yourself) ] ] ]
]

{ #category : #accessing }
DAPackageRelationGraph >> includedPackages [
	"Answer the currently analyzed set of packages."

	^ self packages select: [ :each | each isIncluded ]
]

{ #category : #accessing }
DAPackageRelationGraph >> includesSeenPackage: aPackage [
	^ self seenPackages includes: aPackage
]

{ #category : #initialization }
DAPackageRelationGraph >> initializeWithPackages: aListOfDAPackages [
	self initialize.
	packages := Dictionary new.
	aListOfDAPackages do: [ :each | self addPackage: each ].
	classMapping := IdentityDictionary new
]

{ #category : #testing }
DAPackageRelationGraph >> isReference: aLiteral [
	^ aLiteral isVariableBinding and: [ aLiteral value isBehavior and: [ aLiteral key = aLiteral value name ] ]
]

{ #category : #adding }
DAPackageRelationGraph >> methodsFor: aPackage [
	^ aPackage rPackageSet methods
]

{ #category : #accessing }
DAPackageRelationGraph >> outgoing [
	"Answer a collection of all dependencies that point out of the current package set."

	^ self includedPackages
		inject: Bag new
		into: [ :outgoing :package |
			package dependencies do: [ :each |
				(each isInternal or: [ each target isIncluded ])
					ifFalse: [ outgoing add: each ] ].
			outgoing ]
]

{ #category : #enumerating }
DAPackageRelationGraph >> package: aPackage classesDo: aBlock [
	aPackage rPackageSet classes do: aBlock
]

{ #category : #accessing }
DAPackageRelationGraph >> packageAt: aPackageName [
	"answer a DAPackage"

	^ (packages keys includes: aPackageName)
		ifTrue: [ packages at: aPackageName ]
		ifFalse: [ nil ]
]

{ #category : #queries }
DAPackageRelationGraph >> packageForBehavior: aClass [
	"return a PDPackage and put in classToPackage"

	^ classMapping
		at: aClass
		ifAbsentPut: [
			self packages
				detect: [ :each | each rPackageSet includesClass: aClass ]
				ifNone: [ self systemPackageContaining: aClass ] ]
]

{ #category : #accessing }
DAPackageRelationGraph >> packageForClass: aClass [
	^ packages detect: [ :package | package rPackageSet includesClass: aClass ]
]

{ #category : #accessing }
DAPackageRelationGraph >> packageNamed: aString ifAbsent: aBlock [
	^ packages at: aString ifAbsent: aBlock
]

{ #category : #accessing }
DAPackageRelationGraph >> packages [

	^ packages values
]

{ #category : #accessing }
DAPackageRelationGraph >> packagesName [

	^ packages keys
]

{ #category : #accessing }
DAPackageRelationGraph >> predecessors: aPackage [
	"returns a Collection with the predecessors of the package"

	| aCollection |
	aCollection := OrderedCollection new.
	self packages
		do: [ :package |
			package dependencies
				do: [ :dependency |
					dependency target = aPackage
						ifTrue: [ aCollection add: package ] ] ].
	^ aCollection
]

{ #category : #updating }
DAPackageRelationGraph >> refreshGraph [
	| packageNames |

	packageNames := self seenPackagesName.
	self initializeWithPackages: (packageNames collect: [ :packageName | self daPackageWith: packageName]).
	self build
]

{ #category : #updating }
DAPackageRelationGraph >> refreshPackageDependencies: aPackage [
	aPackage clearDependencies.
	self computeStaticDependencies: aPackage.
	self combineDependencies: aPackage.
	aPackage removeAllInternal.
	aPackage beSeen
]

{ #category : #actions }
DAPackageRelationGraph >> removeInternalDependencies [
	"Remove all dependencies from a package to itself"
	self includedPackages do: [ :each | each removeAllInternal ]
]

{ #category : #actions }
DAPackageRelationGraph >> removeOutgoingDependencies [
	"Remove all dependencies to packages not included in this relation"

	self includedPackages
		do: [ :each | self removeOutgoingDependencies: each ]
		displayingProgress: 'Removing outgoing Dependencies'
]

{ #category : #private }
DAPackageRelationGraph >> removeOutgoingDependencies: aPackage [
	| removals |
	removals := aPackage dependencies reject: [ :each | each target isIncluded ].
	removals do: [ :each | aPackage remove: each ]
]

{ #category : #removing }
DAPackageRelationGraph >> removePackage: packageName [
	^ packages removeKey: packageName
]

{ #category : #actions }
DAPackageRelationGraph >> seenPackages [
	^ self packages
		select: [ :each | each isSeen ]
]

{ #category : #accessing }
DAPackageRelationGraph >> seenPackagesName [
	"Answer all seen packages name."

	^ self seenPackages collect: [ :package | package packageName ]
]

{ #category : #actions }
DAPackageRelationGraph >> seenPackagesWithFilter: aFilter [
	^ self packages
		select: [ :each | each isSeen and: [ aFilter match: each packageName ] ]
]

{ #category : #actions }
DAPackageRelationGraph >> seenPackagesWithoutExtension [
	^ self packages select: [ :each | each isSeen ]
]

{ #category : #accessing }
DAPackageRelationGraph >> shouldComputeMessageSendDependencies [

	^ shouldComputeMessageSendDependencies ifNil: [ true ]
]

{ #category : #accessing }
DAPackageRelationGraph >> shouldComputeMessageSendDependencies: aBoolean [

	shouldComputeMessageSendDependencies := aBoolean
]

{ #category : #accessing }
DAPackageRelationGraph >> successors: aPackage [
	"answer a collection with the successors of aPackage in the graph"

	| aCollection |
	aCollection := OrderedCollection new.
	(packages at: aPackage packageName) dependencies
		do: [ :dependency |
			(self packagesName includes: dependency target packageName)
				ifTrue: [ aCollection add: dependency target ] ].
	^ aCollection
]

{ #category : #queries }
DAPackageRelationGraph >> systemPackageContaining: aClass [

	| packageName |
	packageName := (Smalltalk organization packageOf: aClass) packageName.
	^ packageName
		  ifNil: [ self error: 'Package for ' , aClass name , ' not found.' ]
		  ifNotNil: [ DAPackage on: (RPackageSet named: packageName asString) ]
]

{ #category : #computing }
DAPackageRelationGraph >> usersOf: aPackage [
	| users |
	users := OrderedCollection new.
	self seenPackages
		do: [ :package |
			(package dependentPackages includes: aPackage)
				ifTrue: [ users add: package ] ].
	^ users
]
