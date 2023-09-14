"
Common superclass for RPackage related tests
"
Class {
	#name : #RPackageTestCase,
	#superclass : #TestCase,
	#instVars : [
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

	^ #( 'MockPackage-Tests' 'MockPackage-tests' 'MockPackage' 'MockPackage-Tests-Package' ) collect: [ :pName | self organizer ensurePackage: pName ]
]

{ #category : #utilities }
RPackageTestCase >> createNewClassNamed: aName inCategory: cat [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: cat ]
]

{ #category : #utilities }
RPackageTestCase >> createNewClassNamed: aName inPackage: p [

	^ self createNewClassNamed: aName inCategory: p name
]

{ #category : #utilities }
RPackageTestCase >> createNewTraitNamed: aName inCategory: cat [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  package: cat;
			  installingEnvironment: testEnvironment;
			  beTrait ]
]

{ #category : #utilities }
RPackageTestCase >> createNewTraitNamed: aName inPackage: p [

	^ self createNewTraitNamed: aName inCategory: p name
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
RPackageTestCase >> tearDown [

	self organizer packages do: [ :package |
		self allWorkingCopies
			detect: [ :mcPackage | mcPackage packageName = package packageName asString ]
			ifFound: [ :mcPackage | mcPackage unregister ] ].
	super tearDown
]
