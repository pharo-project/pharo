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

{ #category : 'testing' }
RPackageTestCase class >> isAbstract [

	^ self = RPackageTestCase
]

{ #category : 'private' }
RPackageTestCase >> createMethodNamed: methodName inClass: aClass asExtensionOf: aPackage [

	^ aClass compile: methodName , ' ^nil' classified: '*' , (aPackage isString
			   ifTrue: [ aPackage ]
			   ifFalse: [ aPackage name ])
]

{ #category : 'private' }
RPackageTestCase >> createMethodNamed: methodName inClass: aClass inProtocol: aProtocol [ 

	^ aClass compile: (methodName, ' ^nil') classified: aProtocol.
]

{ #category : 'helpers' }
RPackageTestCase >> ensurePackage: aName [

	^ self organizer ensurePackage: aName
]

{ #category : 'helpers' }
RPackageTestCase >> ensureTagYinX [

	^ self organizer ensureTag: #YYYY inPackage: #XXXXX
]

{ #category : 'helpers' }
RPackageTestCase >> ensureXPackage [

	^ self organizer ensurePackage: self xPackageName
]

{ #category : 'helpers' }
RPackageTestCase >> ensureYPackage [

	^ self organizer ensurePackage: self yPackageName
]

{ #category : 'helpers' }
RPackageTestCase >> ensureZPackage [

	^ self organizer ensurePackage: self zPackageName
]

{ #category : 'helpers' }
RPackageTestCase >> newClassNamed: aName in: aPackage [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: (aPackage isString
					   ifTrue: [ aPackage ]
					   ifFalse: [ aPackage name ]) ]
]

{ #category : 'helpers' }
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

{ #category : 'helpers' }
RPackageTestCase >> newClassNamed: aName inTag: aTag [

	^ self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  installingEnvironment: testEnvironment;
			  package: aTag package name;
			  tag: aTag name ]
]

{ #category : 'helpers' }
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

{ #category : 'helpers' }
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

{ #category : 'helpers' }
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

{ #category : 'helpers' }
RPackageTestCase >> xPackageName [

	^ #XXXXX
]

{ #category : 'helpers' }
RPackageTestCase >> yPackageName [

	^ #YYYYY
]

{ #category : 'helpers' }
RPackageTestCase >> zPackageName [

	^ #ZZZZZ
]
