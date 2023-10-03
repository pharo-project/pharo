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
RPackageTestCase >> ensurePackage: aName [

	^ self organizer ensurePackage: aName
]

{ #category : 'utilities' }
RPackageTestCase >> newClassNamed: aName in: aPackage [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]) ]
]

{ #category : 'utilities' }
RPackageTestCase >> newClassNamed: aName in: aPackage tag: aTag [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]);
			  tag: (aTag isString
					   ifTrue: [ aTag ]
					   ifFalse: [ aTag name ]) ]
]

{ #category : 'utilities' }
RPackageTestCase >> newClassNamed: aName inTag: aTag [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: aTag package name;
			  tag: aTag name ]
]

{ #category : 'utilities' }
RPackageTestCase >> newTraitNamed: aName in: aPackage [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]);
			  installingEnvironment: testEnvironment;
			  beTrait ]
]

{ #category : 'utilities' }
RPackageTestCase >> newTraitNamed: aName inTag: aTag [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  package: aTag package name;
			  tag: aTag name;
			  installingEnvironment: testEnvironment;
			  beTrait ]
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
