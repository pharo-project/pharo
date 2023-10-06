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
PackageAnnouncementsTest >> testAddClassAnnounceClassRecategorized [

	| xPackage yPackage class |
	self flag: #package. "This announcement should be removed in the future."
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	class := self newClassNamed: #NewClass in: xPackage.

	self when: ClassRecategorized do: [ :ann | self assert: ann classRecategorized name equals: #NewClass ].

	yPackage addClass: class.

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
