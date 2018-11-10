"
I'm a reader for tonel format.
"
Class {
	#name : #TonelReader,
	#superclass : #MCVersionReader,
	#instVars : [
		'packageDirectory'
	],
	#category : #'MonticelloTonel-Core'
}

{ #category : #testing }
TonelReader class >> canReadFileNamed: fileName [
	"We validate that fileName does not contains dot, but that's the only thing we can do 
	 at this point."
	^ fileName noneSatisfy: [ :each | each = $. ]
]

{ #category : #accessing }
TonelReader class >> definitionOrders [
	"Used to sort definitions inside a snapshot"
	^ Dictionary newFromPairs: { 
		MCOrganizationDefinition. 	0.
		MCMethodDefinition. 			1.
		MCClassDefinition. 			2.
		MCTraitDefinition. 			2.
		MCClassTraitDefinition. 		3 }
]

{ #category : #accessing }
TonelReader class >> extension [
	"This is needed because the implementation of MCCacheRepository assumes each package 
	 has an extension (which is a lame implementation) and then we need to provide one. 
	 Since tonel is not cached (because we have our oen cache in the form of a local repository), 
	 we put something that will not be found anyway"
	^ 'NOTUSED'
		
]

{ #category : #testing }
TonelReader class >> isAbstract [
	^ false
]

{ #category : #reading }
TonelReader class >> on: stream fileName: fileName [
	^ (self on: stream)
		packageDirectory: fileName;
		yourself
]

{ #category : #private }
TonelReader >> canBeLoaded: aFileReference [
	| fileName |
	fileName := self fileUtils fileName: aFileReference. 
	^ fileName ~= 'package.st' and: [ fileName endsWith: '.st' ]
]

{ #category : #private }
TonelReader >> categoriesFrom: aCollection [
	^ ((aCollection select: #isClassDefinition)
		collect: #category
		as: Set)
		sorted: [ :a :b | a < b ]
]

{ #category : #accessing }
TonelReader >> fileUtils [
	^ TonelFileUtils current
]

{ #category : #loading }
TonelReader >> loadDefinitions [
	| definitionOrders newDefinitions newCategories organization |
	
	definitionOrders := self class definitionOrders.
	newDefinitions := ((self packageDirectory entries
		select: [ :each | self canBeLoaded: each ]) 
		collect: [ :each | each readStreamDo: [ :s | TonelParser parseStream: s ] ])
		flattened
		sort: [ :a :b | (definitionOrders at: a class) < (definitionOrders at: b class) ].

	newCategories := self categoriesFrom: newDefinitions.
	(newCategories includes: self package name asSymbol)
		ifFalse: [ newCategories := newCategories copyWithFirst: self package name ].
	organization := MCOrganizationDefinition categories: newCategories.
	
	definitions := (newDefinitions copyWithFirst: organization) sort
]

{ #category : #loading }
TonelReader >> loadDependencies [
	"There should not be dependencies"
	dependencies := #()
]

{ #category : #loading }
TonelReader >> loadPackage [
	package := MCPackage named: self packageNameFromPackageDirectory
]

{ #category : #loading }
TonelReader >> loadVersionInfo [
	info := MCVersionInfo
		name: self packageNameFromPackageDirectory, '-tonel.1'
		id: UUID new
		message: 'fabricated from a Tonel format repository'
		date: Date today
		time: Time now
		author: ''
		ancestors: #()
		stepChildren: #()
]

{ #category : #accessing }
TonelReader >> packageDirectory [
	^ packageDirectory
]

{ #category : #accessing }
TonelReader >> packageDirectory: aDirectoryName [
	packageDirectory := self fileUtils 
		directoryFromPath: aDirectoryName 
		relativeTo: stream
]

{ #category : #private }
TonelReader >> packageNameFromPackageDirectory [
    ^ packageDirectory basename
]
