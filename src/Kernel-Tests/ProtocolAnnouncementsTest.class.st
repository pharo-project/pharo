"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : #ProtocolAnnouncementsTest,
	#superclass : #ProtocolTest,
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #running }
ProtocolAnnouncementsTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : #tests }
ProtocolAnnouncementsTest >> testAddProtocolAnnouncement [

	| gotAnnouncement |
	gotAnnouncement := false.
	SystemAnnouncer uniqueInstance
		when: ProtocolAdded
		do: [ :ann |
			gotAnnouncement := true.
			self assert: ann protocol name equals: #king.
			self assert: ann classReorganized name equals: self classNameForTests ]
		for: self.
	organization addProtocol: #king.
	self assert: gotAnnouncement
]
