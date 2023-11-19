"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : 'MethodAnnouncementsTest',
	#superclass : 'AbstractClassDescriptionAnnouncementTest',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
MethodAnnouncementsTest >> testCompileMethodAnnounceAddition [

	self when: MethodAdded do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann method protocol name equals: #titan.
		self assert: ann methodClass name equals: self classNameForTests.
		self assert: ann methodPackage name equals: self packageNameForTests.
		self assert: ann packagesAffected equals: { self packageNameForTests asPackage } ].

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
MethodAnnouncementsTest >> testRemoveMethodAnnounceRemoval [

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self when: MethodRemoved do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann method protocol name equals: #titan.
		self assert: ann methodClass name equals: self classNameForTests.
		"Until Pharo 12 the MethodRemoved announcement did not know the package of the method."
		self assert: ann methodPackage name equals: self packageNameForTests.
		self assert: ann packagesAffected equals: { self packageNameForTests asPackage } ].

	class removeSelector: #king.

	self assert: numberOfAnnouncements equals: 1
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
MethodAnnouncementsTest >> testUpdateMethodAnnounceModification [

	class compiler
		protocol: #titan;
		install: 'king ^ 2'.

	self when: MethodModified do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann method protocol name equals: #titan.
		self assert: ann methodClass name equals: self classNameForTests.
		self assert: ann methodPackage name equals: self packageNameForTests.
		self assert: ann packagesAffected equals: { self packageNameForTests asPackage } ].

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
MethodAnnouncementsTest >> testUpdateMethodAnnounceRecategorization [

	class compiler
		protocol: #demon;
		install: 'king ^ 2'.

	self when: MethodRecategorized do: [ :ann |
		self assert: ann method selector equals: #king.
		self assert: ann oldProtocol name equals: #demon.
		self assert: ann newProtocol name equals: #titan.
		self assert: ann methodClass name equals: self classNameForTests.
		self assert: ann methodPackage name equals: self packageNameForTests.
		self assert: ann packagesAffected equals: { self packageNameForTests asPackage } ].

	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self assert: numberOfAnnouncements equals: 1
]
