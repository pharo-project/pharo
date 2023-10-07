"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : 'MethodAnnouncementsTest',
	#superclass : 'AbstractClassDescriptionTest',
	#instVars : [
		'numberOfAnnouncements'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'running' }
MethodAnnouncementsTest >> setUp [

	super setUp.
	numberOfAnnouncements := 0
]

{ #category : 'running' }
MethodAnnouncementsTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : 'tests' }
MethodAnnouncementsTest >> testRemoveSelectorDoesNotAnnounceRecategorization [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self when: MethodRecategorized do: [ :ann | self fail: 'No method recategorization should happen during the removal of a method' ].

	self when: MethodRemoved do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann methodClass name equals: self classNameForTests.
		self assert: ann methodPackage name equals: self packageNameForTests ].

	class removeSelector: #king.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
MethodAnnouncementsTest >> testRemoveSelectorSetThePackage [
	"Until Pharo 12 the MethodRemoved announcement did not know the package of the method."

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self when: MethodRemoved do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann methodClass name equals: self classNameForTests.
		self assert: ann methodPackage name equals: self packageNameForTests ].

	class removeSelector: #king.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'helpers' }
MethodAnnouncementsTest >> when: anAnnouncement do: aBlock [

	SystemAnnouncer uniqueInstance
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
