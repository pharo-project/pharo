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

{ #category : #helpers }
ProtocolAnnouncementsTest >> when: anAnnouncement do: aBlock [

	SystemAnnouncer uniqueInstance
		when: anAnnouncement
		do: [ :ann |
			numberOfAnnouncements := numberOfAnnouncements + 1.
			aBlock cull: ann ]
		for: self
]
