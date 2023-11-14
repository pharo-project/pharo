"
Common superclass for Package related tests
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

{ #category : 'private' }
RPackageTestCase >> createMethodNamed: methodName inClass: aClass inCategory: aCategoryName [ 

	^ aClass compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : 'private' }
RPackageTestCase >> createMethodNamed: methodName inClassSideOfClass: aClass inCategory: aCategoryName [ 

	^ aClass classSide compile: (methodName, ' ^nil') classified: aCategoryName.
]

{ #category : 'utilities' }
RPackageTestCase >> ensurePackage: aName [

	^ self organizer ensurePackage: aName
]

{ #category : 'utilities' }
RPackageTestCase >> ensureTagYinX [

	^ self organizer ensureTag: #YYYY inPackage: #XXXXX
]

{ #category : 'utilities' }
RPackageTestCase >> ensureXPackage [

	^ self organizer ensurePackage: #XXXXX
]

{ #category : 'utilities' }
RPackageTestCase >> ensureYPackage [

	^ self organizer ensurePackage: #YYYYY
]

{ #category : 'utilities' }
RPackageTestCase >> ensureZPackage [

	^ self organizer ensurePackage: #ZZZZZ
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
RPackageTestCase >> newTraitNamed: aName in: aPackage tag: aTag [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  beTrait;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]);
			  tag: (aTag isString
					   ifTrue: [ aTag ]
					   ifFalse: [ aTag name ]) ]
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
RPackageTestCase >> setUp [

	super setUp.
	testEnvironment := self class environment class new
]

{ #category : 'running' }
RPackageTestCase >> tearDown [

	self organizer environment allClasses do: [ :cls | cls removeFromSystem ].
	self organizer packages do: [ :package | package removeFromSystem ].

	super tearDown
]
