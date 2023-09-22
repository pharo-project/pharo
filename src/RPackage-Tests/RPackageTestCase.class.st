"
Common superclass for RPackage related tests
"
Class {
	#name : 'RPackageTestCase',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'testEnvironment'
	],
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'utilities' }
RPackageTestCase >> createMockTestPackages [

	^ self namesOfMockTestPackages collect: [:pName | self ensurePackage: pName]
]

{ #category : 'utilities' }
RPackageTestCase >> createNewClassNamed: aName [
	^ self createNewClassNamed: aName inCategory: 'RPackageTest'
]

{ #category : 'utilities' }
RPackageTestCase >> createNewClassNamed: aName inCategory: cat [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: cat ]
]

{ #category : 'utilities' }
RPackageTestCase >> createNewClassNamed: aName inPackage: p [

	| cls |
	cls := self createNewClassNamed: aName inCategory: p name.
	p importClass: cls.
	^ cls
]

{ #category : 'utilities' }
RPackageTestCase >> createNewTraitNamed: aName [
	^ self createNewTraitNamed: aName inCategory: 'RPackageTest'
]

{ #category : 'utilities' }
RPackageTestCase >> createNewTraitNamed: aName inCategory: cat [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  package: cat;
			  installingEnvironment: testEnvironment;
			  beTrait ]
]

{ #category : 'utilities' }
RPackageTestCase >> createNewTraitNamed: aName inPackage: p [

	| cls |
	cls := self createNewTraitNamed: aName.
	p importClass: cls.
	^ cls
]

{ #category : 'utilities' }
RPackageTestCase >> ensurePackage: aName [

	^ self organizer ensurePackage: aName
]

{ #category : 'utilities' }
RPackageTestCase >> namesOfMockTestPackages [

	^ #( 'MockPackage-Tests' 'MockPackage2-tests' 'MockPackage' 'MockPackage-Tests-Package')
]

{ #category : 'accessing' }
RPackageTestCase >> organizer [

	^ testEnvironment organization
]

{ #category : 'running' }
RPackageTestCase >> runCase [

	testEnvironment := self class environment class new.
	[
	^ RPackage withOrganizer: self organizer do: [
		  self resources do: [ :each | each availableFor: self ].
		  self setUp.
		  self performTest ] ] ensure: [
		self tearDown.
		self cleanUpInstanceVariables ]
]

{ #category : 'running' }
RPackageTestCase >> tearDown [

	self organizer environment allClasses do: [ :cls | cls removeFromSystem ].

	super tearDown
]
