"
Common superclass for RPackage related tests
"
Class {
	#name : 'PackageTestCase',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'testEnvironment'
	],
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'testing' }
PackageTestCase class >> isAbstract [

	^ self = PackageTestCase
]

{ #category : 'private' }
PackageTestCase >> createMethodNamed: methodName inClass: aClass asExtensionOf: aPackage [

	^ aClass compile: methodName , ' ^nil' classified: '*' , (aPackage isString
			   ifTrue: [ aPackage ]
			   ifFalse: [ aPackage name ])
]

{ #category : 'private' }
PackageTestCase >> createMethodNamed: methodName inClass: aClass inProtocol: aProtocol [ 

	^ aClass compile: (methodName, ' ^nil') classified: aProtocol.
]

{ #category : 'helpers' }
PackageTestCase >> ensurePackage: aName [

	^ self organizer ensurePackage: aName
]

{ #category : 'helpers' }
PackageTestCase >> ensureTagYinX [

	^ self organizer ensureTag: #YYYY inPackage: #XXXXX
]

{ #category : 'helpers' }
PackageTestCase >> ensureXPackage [

	^ self organizer ensurePackage: self xPackageName
]

{ #category : 'helpers' }
PackageTestCase >> ensureYPackage [

	^ self organizer ensurePackage: self yPackageName
]

{ #category : 'helpers' }
PackageTestCase >> ensureZPackage [

	^ self organizer ensurePackage: self zPackageName
]

{ #category : 'helpers' }
PackageTestCase >> newClassNamed: aName in: aPackage [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]) ]
]

{ #category : 'helpers' }
PackageTestCase >> newClassNamed: aName in: aPackage tag: aTag [

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

{ #category : 'helpers' }
PackageTestCase >> newClassNamed: aName inTag: aTag [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: aTag package name;
			  tag: aTag name ]
]

{ #category : 'helpers' }
PackageTestCase >> newTraitNamed: aName in: aPackage [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]);
			  installingEnvironment: testEnvironment;
			  beTrait ]
]

{ #category : 'helpers' }
PackageTestCase >> newTraitNamed: aName in: aPackage tag: aTag [

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

{ #category : 'helpers' }
PackageTestCase >> newTraitNamed: aName inTag: aTag [

	^ self class classInstaller make: [ :aBuilder |
		  aBuilder
			  name: aName;
			  package: aTag package name;
			  tag: aTag name;
			  installingEnvironment: testEnvironment;
			  beTrait ]
]

{ #category : 'accessing' }
PackageTestCase >> organizer [

	^ testEnvironment organization
]

{ #category : 'running' }
PackageTestCase >> setUp [

	super setUp.
	testEnvironment := self class environment class new
]

{ #category : 'running' }
PackageTestCase >> tearDown [

	self organizer environment allClasses do: [ :cls | cls removeFromSystem ].
	self organizer packages do: [ :package | package removeFromSystem ].

	super tearDown
]

{ #category : 'helpers' }
PackageTestCase >> xPackageName [

	^ #XXXXX
]

{ #category : 'helpers' }
PackageTestCase >> yPackageName [

	^ #YYYYY
]

{ #category : 'helpers' }
PackageTestCase >> zPackageName [

	^ #ZZZZZ
]
