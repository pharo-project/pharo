Class {
	#name : 'PackageAnnouncementsTest',
	#superclass : 'PackageTestCase',
	#instVars : [
		'numberOfAnnouncements'
	],
	#category : 'Kernel-CodeModel-Tests-Packages',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Packages'
}

{ #category : 'running' }
PackageAnnouncementsTest >> setUp [

	super setUp.
	numberOfAnnouncements := 0
]

{ #category : 'running' }
PackageAnnouncementsTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : 'tests - classes' }
PackageAnnouncementsTest >> testAddClassAnnounceClassRepackaged [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: #NewClass in: xPackage.

	self when: ClassRepackaged do: [ :ann |
		self assert: ann classRepackaged name equals: #NewClass.
		self assert: ann newPackage identicalTo: yPackage.
		self assert: ann oldPackage identicalTo: xPackage ].

	yPackage addClass: class.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testAddExtensionMethodHasTheRightPackage [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: #NewClass1 in: xPackage.

	self when: MethodAdded do: [ :ann |
		self assert: ann methodAdded selector equals: #testMethod.
		self assert: ann methodClass identicalTo: class.
		self assert: ann methodPackage identicalTo: yPackage ].

	class compile: 'testMethod ^ #kheego' classified: '*' , yPackage name.
	
	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - classes' }
PackageAnnouncementsTest >> testClassRepackagedNotFiredAtAClassAddition [

	| xPackage class |
	xPackage := self ensureXPackage.

	self when: ClassRepackaged do: [ :ann |
		self fail: 'When we are packaging the class for the first time, we should not raise a class repackaged because we already raise a class added with the package known.' ].

	self when: ClassAdded do: [ :ann |
		self assert: ann classAffected name equals: #NewClass.
		self assert: ann packageAffected equals: xPackage ].

	class := self newClassNamed: #NewClass in: xPackage.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testMethodCategorizedInSameClassicProtocolDoesNotRepackage [

	| class |
	class := self newClassNamed: 'NewClass' in: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic'.

	self when: MethodRepackaged do: [ :a | self fail: 'The announcement should not be raised.' ].

	class classify: #stubMethod under: 'another classic one'.

	self assert: numberOfAnnouncements equals: 0
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testMethodCategorizedInSameExtensionDoesNotRepackage [

	| class |
	class := self newClassNamed: 'NewClass' in: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: 'yyyyy'.

	self when: MethodRepackaged do: [ :a | self fail: 'The announcement should not be raised.' ].

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: numberOfAnnouncements equals: 0
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testMoveClassToTagInNewPackage [

	| package1 package2 tag1 tag2 class |
	package1 := self organizer ensurePackage: #Package1.
	package2 := self organizer ensurePackage: #Package2.
	tag1 := package1 ensureTag: #Tag1.
	tag2 := package2 ensureTag: #Tag2.

	class := self newClassNamed: #TestClassInP1 inTag: tag1.

	self when: ClassRepackaged do: [ :ann |
		self assert: ann classAffected name equals: #TestClassInP1.
		self assert: ann newPackage equals: package2.
		self assert: ann oldPackage equals: package1.
		self assert: ann newTag equals: tag2.
		self assert: ann oldTag equals: tag1 ].

	package2 moveClass: class toTag: tag2.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testMoveMethodFromExtensionToClassicProtocolAnnounceRepackaged [

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	self when: MethodRepackaged do: [ :ann |
		self assert: ann methodRepackaged selector equals: #stubMethod.
		self assert: ann newPackage identicalTo: xPackage.
		self assert: ann oldPackage identicalTo: yPackage ].

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class classify: #stubMethod under: 'classic one'.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testMoveMethodFromExtensionToExtensionAnnounceRepackaged [

	| class xPackage yPackage zPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	self when: MethodRepackaged do: [ :ann |
		self assert: ann methodRepackaged selector equals: #stubMethod.
		self assert: ann newPackage identicalTo: zPackage.
		self assert: ann oldPackage identicalTo: yPackage ].

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class asExtensionOf: yPackage.

	class classify: #stubMethod under: '*' , self zPackageName.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - methods' }
PackageAnnouncementsTest >> testMoveMethodToExtensionAnnounceRepackaged [

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	self when: MethodRepackaged do: [ :ann |
		self assert: ann methodRepackaged selector equals: #stubMethod.
		self assert: ann newPackage identicalTo: yPackage.
		self assert: ann oldPackage identicalTo: xPackage ].

	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inProtocol: 'classic category'.

	class classify: #stubMethod under: '*yyyyy'.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - classes' }
PackageAnnouncementsTest >> testSetPackageOfClassAnnounceClassRepackaged [

	| xPackage yPackage class |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: #NewClass in: xPackage.

	self when: ClassRepackaged do: [ :ann |
		self assert: ann classRepackaged name equals: #NewClass.
		self assert: ann newPackage identicalTo: yPackage.
		self assert: ann oldPackage identicalTo: xPackage ].

	class package: yPackage.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'running' }
PackageAnnouncementsTest >> when: anAnnouncement do: aBlock [

	SystemAnnouncer uniqueInstance
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
