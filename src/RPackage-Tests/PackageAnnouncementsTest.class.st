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
PackageAnnouncementsTest >> testMoveClassToTagInNewPackage [

	| package1 package2 class |
	package1 := self organizer ensurePackage: #Package1.
	package2 := self organizer ensurePackage: #Package2.

	class := self newClassNamed: #TestClassInP1 in: package1.

	self when: ClassRepackaged do: [ :ann |
		self assert: ann classAffected name equals: #TestClassInP1.
		self assert: ann newPackage equals: package2.
		self assert: ann oldPackage equals: package1 ].

	package2 moveClass: class toTag: (package2 ensureTag: #TAG).
	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
PackageAnnouncementsTest >> testMoveClassToTagInTagOfTheSamePackage [

	| package1 class |
	package1 := self organizer ensurePackage: #Package1.

	class := self newClassNamed: #TestClassInP1 in: package1.

	self when: ClassRepackaged do: [ :ann | self fail: 'No repackaging should happen.' ].

	package1 moveClass: class toTag: (package1 ensureTag: #TAG).
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
