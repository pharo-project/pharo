"
I am a RBBrowserEnvironment on a set of packages or package names.
I containt all entities are defined in this packages.
(classes and class that have extensions from this packages)

Example:
(RBBrowserEnvironment new) forPackageNames:{ 'Morphic-Base'}.
"
Class {
	#name : #RBPackageEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'packages'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBPackageEnvironment class >> onEnvironment: anEnvironment packageNames: aCollection [
	^ self 
		onEnvironment: anEnvironment
		packages: (aCollection collect: [ :each | 
			RPackageOrganizer default packageNamed: each ])
]

{ #category : #'instance creation' }
RBPackageEnvironment class >> onEnvironment: anEnvironment packages: aCollection [
	^ (self onEnvironment: anEnvironment)
		packages: aCollection;
		yourself
]

{ #category : #'instance creation' }
RBPackageEnvironment class >> package: aPackage [
	^ self packages: { aPackage }
]

{ #category : #'instance creation' }
RBPackageEnvironment class >> packageName: aName [
	^ self packageNames: { aName }
]

{ #category : #'instance creation' }
RBPackageEnvironment class >> packageNames: aCollection [
	^ self 
		onEnvironment: self default
		packageNames: aCollection
]

{ #category : #'instance creation' }
RBPackageEnvironment class >> packages: aCollection [
	^ self 
		onEnvironment: self default
		packages: aCollection
]

{ #category : #adding }
RBPackageEnvironment >> addPackage: aSymbol [
	packages add: aSymbol
]

{ #category : #accessing }
RBPackageEnvironment >> asSelectorEnvironment [
	| result |
	result := RBSelectorEnvironment onEnvironment: environment.
	self classesAndSelectorsDo: [ :class :selector | result addClass: class selector: selector ].
	^ result
]

{ #category : #accessing }
RBPackageEnvironment >> classesAndSelectorsDo: aBlock [
	packages do: [ :package |
		package methods do: [ :method |
			(environment includesSelector: method selector in: method methodClass)
				ifTrue: [ aBlock value: method methodClass value: method selector ] ] ]
]

{ #category : #accessing }
RBPackageEnvironment >> classesDo: aBlock [
	| enumerated enumerator |
	enumerated := IdentitySet new.
	enumerator := [ :each |
		(enumerated includes: each) ifFalse: [
			(environment includesClass: each)
				ifTrue: [ aBlock value: each ].
			(environment includesClass: each classSide)
				ifTrue: [ aBlock value: each classSide].
			enumerated add: each ] ].
	packages do: [ :package |
		package classes do: enumerator.
		package extendedClasses do: enumerator ]
]

{ #category : #testing }
RBPackageEnvironment >> definesClass: aClass [
	^ (super definesClass: aClass) and: [ self packages anySatisfy: [ :package | package includesClass: aClass ] ]
]

{ #category : #testing }
RBPackageEnvironment >> includesCategory: aCategory [
	^ (super includesCategory: aCategory) and: [ 
		self packages anySatisfy: [ :package | 
			package includesSystemCategory: aCategory ] ]
]

{ #category : #testing }
RBPackageEnvironment >> includesClass: aClass [
	^ (super includesClass: aClass) and: [ 
		self packages anySatisfy: [ :package | 
			(package includesClass: aClass) or: [ 
			(package extensionCategoriesForClass: aClass) notEmpty ]]]
]

{ #category : #testing }
RBPackageEnvironment >> includesProtocol: aProtocol in: aClass [
	^ (environment includesProtocol: aProtocol in: aClass) and: [ self packages anySatisfy: [ :package | package includesMethodCategory: aProtocol ofClass: aClass ] ]
]

{ #category : #testing }
RBPackageEnvironment >> includesSelector: aSelector in: aClass [
	^ (environment includesSelector: aSelector in: aClass) 
		and: [ self packages anySatisfy: [ :package | 
				package includesSelector: aSelector ofClass: aClass ] ]
]

{ #category : #initialization }
RBPackageEnvironment >> initialize [
	super initialize.
	packages := Set new
]

{ #category : #accessing }
RBPackageEnvironment >> packageNames [
	^ packages collect: [ :each | each packageName ]
]

{ #category : #accessing }
RBPackageEnvironment >> packages [
	^ packages
]

{ #category : #initialization }
RBPackageEnvironment >> packages: aCollection [
	packages addAll: aCollection
]

{ #category : #copying }
RBPackageEnvironment >> postCopy [
	super postCopy.
	packages := packages copy
]

{ #category : #printing }
RBPackageEnvironment >> printOn: aStream [ 
	self storeOn: aStream
]

{ #category : #printing }
RBPackageEnvironment >> storeOn: aStream [ 
	aStream nextPut: $(; nextPutAll: self class name; nextPutAll: ' onEnvironment: '.
	environment storeOn: aStream.
	aStream nextPutAll: ' packageNames: '.
	self packageNames asArray storeOn: aStream.
	aStream nextPut: $)
]
