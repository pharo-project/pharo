Class {
	#name : 'PackageAnnouncementsTest',
	#superclass : 'RPackageTestCase',
	#instVars : [
		'numberOfAnnouncements'
	],
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'running' }
PackageAnnouncementsTest >> when: anAnnouncement do: aBlock [

	SystemAnnouncer uniqueInstance
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
