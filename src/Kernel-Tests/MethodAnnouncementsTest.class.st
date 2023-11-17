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
