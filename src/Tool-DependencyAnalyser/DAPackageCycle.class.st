"
A PDPackageCycle is used to represent a cycle among packages. 	
All the packages in the cycle are stored in a Collection, in the their name alphabetical order.

As soon as possible, they will be stored in the order they have been found.
"
Class {
	#name : #DAPackageCycle,
	#superclass : #Object,
	#instVars : [
		'packages'
	],
	#category : #Tool-DependencyAnalyser-Core
}

{ #category : #'instance creation' }
DAPackageCycle class >> new [
	^ self basicNew initialize; yourself.
]

{ #category : #comparing }
DAPackageCycle >> <= aPDPackageCycle [
	^ (self size) <= (aPDPackageCycle size)

]

{ #category : #comparing }
DAPackageCycle >> = aPDPackageCycle [
	(aPDPackageCycle isKindOf: self class) ifFalse: [ ^ false ]. 
	^ self size = aPDPackageCycle size and: [ self packages includesAll: aPDPackageCycle packages ].
]

{ #category : #comparing }
DAPackageCycle >> > aPDPackageCycle [
	^ (self size) > (aPDPackageCycle size)

]

{ #category : #adding }
DAPackageCycle >> addAllPackages: aCollection [
	
	aCollection do: [ :package | self addPackage: package].
]

{ #category : #adding }
DAPackageCycle >> addPackage: aPDPackage [
	packages add: aPDPackage
]

{ #category : #accessing }
DAPackageCycle >> dependentPackagesFrom: aPackage [
	"only the dependent packages from the cycle"
	^ aPackage dependentPackages select: [ :package |
		self packages includes: package
		 ]
]

{ #category : #displaying }
DAPackageCycle >> displayString [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'A cycle of ';
			print: self size;
			nextPutAll: ' packages : '.
		packages
			do: [ :package | stream nextPutAll: package packageName ]
			separatedBy: [ stream space ] ]
]

{ #category : #testing }
DAPackageCycle >> hasOutgoingDependencies: aPackage [
	^ aPackage dependencies anySatisfy: [ :dependency | (self packages includes: (dependency target)) not ].
]

{ #category : #comparing }
DAPackageCycle >> hash [
	^ self packages hash.
]

{ #category : #initialization }
DAPackageCycle >> initialize [
	super initialize.
	packages := OrderedCollection new.
]

{ #category : #testing }
DAPackageCycle >> isAtLeastInCycle: aCollectionOfPDPackage [
	^ aCollectionOfPDPackage anySatisfy: [ :package | self isInCycle: package ]
]

{ #category : #testing }
DAPackageCycle >> isInCycle: aPackage [
	^ packages includes: aPackage
]

{ #category : #accessing }
DAPackageCycle >> packageAt: anInteger [
	^ packages at: anInteger
]

{ #category : #accessing }
DAPackageCycle >> packages [
	^ packages
]

{ #category : #accessing }
DAPackageCycle >> packages: anObject [
	packages := anObject
]

{ #category : #actions }
DAPackageCycle >> removeOutgoingDependencies [
	self packages do: [ :package | self removeOutgoingDependencies: package ].
]

{ #category : #private }
DAPackageCycle >> removeOutgoingDependencies: aPackage [
	
	| removals |
	packages := packages copy.
	removals := aPackage dependencies reject: [ :dependency | self packages includes: (dependency target) ].
	
	removals do: [ :each | aPackage remove: each ].
]

{ #category : #accessing }
DAPackageCycle >> reversedCycle [
	packages := packages reversed
]

{ #category : #accessing }
DAPackageCycle >> size [
	^ packages size
]
