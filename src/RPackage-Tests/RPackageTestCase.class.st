"
Common superclass for RPackage related tests
"
Class {
	#name : #RPackageTestCase,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'createdClasses',
		'createdPackages',
		'testEnvironment'
	],
	#category : #'RPackage-Tests'
}

{ #category : #accessing }
RPackageTestCase >> allWorkingCopies [

	^ MCWorkingCopy allWorkingCopies
]

{ #category : #utilities }
RPackageTestCase >> createMockTestPackages [

	^ self namesOfMockTestPackages collect: [:pName | self createNewPackageNamed: pName]
]

{ #category : #utilities }
RPackageTestCase >> createNewClassNamed: aName [
	^ self createNewClassNamed: aName inCategory: 'RPackageTest'
]

{ #category : #utilities }
RPackageTestCase >> createNewClassNamed: aName inCategory: cat [

	| cls |
	cls := self class classInstaller make: [ :aClassBuilder |
		       aClassBuilder
			       name: aName;
			       installingEnvironment: testEnvironment;
			       package: cat ].

	createdClasses add: cls.
	createdPackages add: cls package.
	^ cls
]

{ #category : #utilities }
RPackageTestCase >> createNewClassNamed: aName inPackage: p [

	| cls |
	cls := self createNewClassNamed: aName inCategory: p name.
	p importClass: cls.
	^ cls
]

{ #category : #utilities }
RPackageTestCase >> createNewPackageNamed: aName [

	| pack |
	pack := self organizer ensurePackage: aName.
	createdPackages add: pack.
	^ pack
]

{ #category : #utilities }
RPackageTestCase >> createNewTraitNamed: aName [
	^ self createNewTraitNamed: aName inCategory: 'RPackageTest'
]

{ #category : #utilities }
RPackageTestCase >> createNewTraitNamed: aName inCategory: cat [

	| cls |
	cls := self class classInstaller make: [ :aBuilder |
		       aBuilder
			       name: aName;
			       package: cat;
			       installingEnvironment: testEnvironment;
			       beTrait ].

	createdClasses add: cls.
	createdPackages add: cls package.
	^ cls
]

{ #category : #utilities }
RPackageTestCase >> createNewTraitNamed: aName inPackage: p [

	| cls |
	cls := self createNewTraitNamed: aName.
	p importClass: cls.
	^ cls
]

{ #category : #utilities }
RPackageTestCase >> namesOfMockTestPackages [

	^ #( 'MockPackage-Tests' 'MockPackage-tests' 'MockPackage' 'MockPackage-Tests-Package')
]

{ #category : #accessing }
RPackageTestCase >> organizer [

	^ testEnvironment organization
]

{ #category : #utilities }
RPackageTestCase >> removePackage: aPackage [

	self organizer basicUnregisterPackage: aPackage
]

{ #category : #running }
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

{ #category : #running }
RPackageTestCase >> setUp [

	super setUp.
	createdClasses := Set new.
	createdPackages := Set new
]

{ #category : #running }
RPackageTestCase >> tearDown [

	createdClasses do: [ :cls | cls removeFromSystem ].

	super tearDown
]
