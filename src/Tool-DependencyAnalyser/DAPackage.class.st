"
I am a PDPackage and i represent a package (RPackageSet) and all his dependencies (PDPackageDependency).

Instance variables :

	- dependencies : a collection of PDPackageDependency (actually the nodes of a graph dependency, from a source PDPackage to a target PDPackage)
	- included : says if the PDPackage is included or not in the set of packages at the beginning.
	- rpackage : the instance of asRPackageSet 	
	- inStack : useful for tarjan algorithm and cycle algorithm. It avoid stack access
	- tarjanIndex and tarjanLowLink : integer for the tarjan algorithm.
	- bfsParent : see cycle algorithm
	- seen : says if all dependencies have been added to the Package
"
Class {
	#name : #DAPackage,
	#superclass : #Object,
	#instVars : [
		'dependencies',
		'included',
		'inStack',
		'tarjanIndex',
		'tarjanLowLink',
		'bfsParent',
		'seen',
		'rPackageSet'
	],
	#category : #Tool-DependencyAnalyser-Core
}

{ #category : #'instance creation' }
DAPackage class >> new [
	self shouldNotImplement
]

{ #category : #'instance creation' }
DAPackage class >> on: aRPackageSet [
	^ self basicNew initializeWithPackage: aRPackageSet; yourself
]

{ #category : #'instance creation' }
DAPackage class >> onPackageNamed: aString [
	^ self on: (RPackageSet named: aString)
]

{ #category : #comparing }
DAPackage >> <= aPackage [
	^ (self packageName asString) <= (aPackage packageName asString)
]

{ #category : #comparing }
DAPackage >> = aPackage [
	(aPackage isKindOf: self class)
		ifFalse: [ ^ false ].
	^ self packageName = aPackage packageName
]

{ #category : #adding }
DAPackage >> add: aDependency [
	^ dependencies add: aDependency
]

{ #category : #converting }
DAPackage >> asDependentPackageWrapperOn: relationGraph [
	^ DADependentPackageWrapper newWith: self on: relationGraph
]

{ #category : #converting }
DAPackage >> asPackageDependencyWrapper [
	^ DAPackageDependencyWrapper newWith: self.
]

{ #category : #accessing }
DAPackage >> beIncluded [
	included := true
]

{ #category : #accessing }
DAPackage >> beSeen [
	seen := true
]

{ #category : #accessing }
DAPackage >> bfsParent [
	^ bfsParent
]

{ #category : #accessing }
DAPackage >> bfsParent: aBfsParent [
	bfsParent := aBfsParent
]

{ #category : #initialization }
DAPackage >> clearDependencies [
	dependencies := OrderedCollection new
]

{ #category : #dependencies }
DAPackage >> compositeDependencyTo: aPackage [
	^ self dependencies detect: [ :dependency | dependency target = aPackage ] ifNone: [ nil ]
]

{ #category : #accessing }
DAPackage >> dependencies [
	^ dependencies
]

{ #category : #accessing }
DAPackage >> dependencies: anObject [
	dependencies := anObject
]

{ #category : #accessing }
DAPackage >> dependenciesFrom: aClass [
	| dependenciesFrom |
	dependenciesFrom := OrderedCollection new.
	self dependencies
		do: [ :compositeDependency | 
				dependenciesFrom addAll: (compositeDependency dependenciesFrom: aClass) ].
	^ dependenciesFrom
]

{ #category : #accessing }
DAPackage >> dependenciesSize [

	^ self dependencies 
			inject: 0
			into: [ :sum :dependency |
					sum + dependency numberOfDependencies
				]
]

{ #category : #dependencies }
DAPackage >> dependenciesSizeTo: aPackage [
	| dependency |
	dependency := dependencies detect: [ :each | each target packageName = aPackage packageName ].
	^ dependency ifNotNil: [ dependency numberOfDependencies ] ifNil: [ 0 ]
]

{ #category : #dependencies }
DAPackage >> dependenciesTo: aPackage [
	^ dependencies select: [ :each | each target packageName = aPackage ].
]

{ #category : #dependencies }
DAPackage >> dependentPackages [
	^ self dependencies 
			inject: Set new 
			into: [ :dependents :each | dependents add: each target; yourself ].
]

{ #category : #dependencies }
DAPackage >> dependentPackagesSize [
	^ dependencies size
]

{ #category : #printing }
DAPackage >> displayString [
	| numberOfExtension |
	numberOfExtension := self extensionDependenciesSize.
	^ String streamContents: [ :stream | 
		stream
			nextPutAll: self packageName;
			nextPutAll: ' --> Dependent packages : ';
			nextPutAll: self dependentPackagesSize asString;
			nextPutAll: ' | Dependencies : ';
			nextPutAll: (self dependenciesSize - numberOfExtension) asString;
			nextPutAll: ' (+ ';
			nextPutAll: numberOfExtension asString;
			nextPutAll: ' extension dependencies)' ]
]

{ #category : #accessing }
DAPackage >> extensionDependenciesSize [
	^ self dependencies
		inject: 0
		into: [ :sum :each | sum + each extensionDependenciesSize ]
]

{ #category : #dependencies }
DAPackage >> extensionDependenciesSizeTo: aPackage [
	^ (self compositeDependencyTo: aPackage) extensionDependenciesSize 
]

{ #category : #comparing }
DAPackage >> hash [
	^ self packageName hash
]

{ #category : #accessing }
DAPackage >> inStack: aBoolean [
	inStack := aBoolean
]

{ #category : #accessing }
DAPackage >> included: anObject [
	included := anObject
]

{ #category : #initialization }
DAPackage >> initializeWithPackage: aRPackageSet [
	self initialize.
	self clearDependencies.
	rPackageSet := aRPackageSet.
	included := false.
	inStack := false.
	seen := false.
	tarjanIndex := -1.
	tarjanLowLink := -1.

]

{ #category : #testing }
DAPackage >> isInStack [
	^ inStack
]

{ #category : #testing }
DAPackage >> isIncluded [
	^ included
]

{ #category : #testing }
DAPackage >> isSeen [

	^ seen
]

{ #category : #tarjan }
DAPackage >> isTarjanUndefined [
	^ tarjanIndex = -1
]

{ #category : #accessing }
DAPackage >> packageName [
	^ self rPackageSet packageName
]

{ #category : #printing }
DAPackage >> printOn: aStream [
	aStream
		nextPutAll: 'Dependencies on: ';
		nextPutAll: self packageName
]

{ #category : #accessing }
DAPackage >> rPackage [
	^ self rPackageSet packages first
]

{ #category : #accessing }
DAPackage >> rPackageSet [
	^ rPackageSet
]

{ #category : #accessing }
DAPackage >> rPackageSet: anObject [
	rPackageSet := anObject
]

{ #category : #accessing }
DAPackage >> referenceDependenciesFrom: aClass [

	^ (self dependenciesFrom: aClass) select: [ :each | each isReferenceDependency ]
]

{ #category : #removing }
DAPackage >> remove: aDependency [
	^ dependencies remove: aDependency
]

{ #category : #dependencies }
DAPackage >> removeAllInternal [
	dependencies := dependencies reject: [ :each | each isInternal ]
]

{ #category : #tarjan }
DAPackage >> tarjanIndex [
	^ tarjanIndex 
]

{ #category : #tarjan }
DAPackage >> tarjanIndex: aTarjanIndex [
	tarjanIndex := aTarjanIndex
]

{ #category : #tarjan }
DAPackage >> tarjanLowLink [
	^ tarjanLowLink
]

{ #category : #tarjan }
DAPackage >> tarjanLowLink: aTarjanLowLink [
	tarjanLowLink := aTarjanLowLink.
]
