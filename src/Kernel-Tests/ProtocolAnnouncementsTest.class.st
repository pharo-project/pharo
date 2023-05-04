"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : #ProtocolAnnouncementsTest,
	#superclass : #ProtocolTest,
	#instVars : [
		'numberOfAnnouncements'
	],
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #running }
ProtocolAnnouncementsTest >> setUp [

	super setUp.
	numberOfAnnouncements := 0
]

{ #category : #running }
ProtocolAnnouncementsTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : #tests }
ProtocolAnnouncementsTest >> testAddProtocolAnnouncement [

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #king.
		self assert: ann classReorganized name equals: self classNameForTests ].

	self when: ClassReorganized do: [ :ann | self assert: ann classReorganized name equals: self classNameForTests ].

	organization addProtocol: #king.
	self assert: numberOfAnnouncements equals: 2
]

{ #category : #tests }
ProtocolAnnouncementsTest >> testClassifyUnderAnnounceNewProtocol [

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	self when: ClassReorganized do: [ :ann | self assert: ann classReorganized name equals: self classNameForTests ].

	organization classify: #king under: #titan.
	self assert: numberOfAnnouncements equals: 2
]

{ #category : #tests }
ProtocolAnnouncementsTest >> testRenameProtocolAsAnnounceClassReorganizedOnce [
	"This is a regerssion test because at some point the class reorganized announcement got duplicated."

	organization classify: #king under: #demon.

	self when: ClassReorganized do: [ :ann | self assert: ann classReorganized name equals: self classNameForTests ].

	organization renameProtocol: #demon as: #titan.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : #tests }
ProtocolAnnouncementsTest >> testRenameProtocolAsAnnounceNewProtocol [

	self skip. "This hihglight the problem of https://github.com/pharo-project/pharo/pull/13494
	But we still need to work on this to fix this test."

	organization classify: #king under: #demon.

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	self when: ClassReorganized do: [ :ann | self assert: ann classReorganized name equals: self classNameForTests ].

	organization renameProtocol: #demon as: #titan.

	self assert: numberOfAnnouncements equals: 2
]

{ #category : #helpers }
ProtocolAnnouncementsTest >> when: anAnnouncement do: aBlock [

	SystemAnnouncer uniqueInstance
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
