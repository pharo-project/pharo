"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : #ProtocolAnnouncementsTest,
	#superclass : #TestCase,
	#instVars : [
		'class',
		'organization'
	],
	#category : #'Kernel-Tests-Protocols'
}

{ #category : #running }
ProtocolAnnouncementsTest >> classNameForTests [

	^ #ClassForTests
]

{ #category : #running }
ProtocolAnnouncementsTest >> setUp [

	super setUp.
	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: self classNameForTests;
			         package: 'ClassOrganizer-Tests' ].

	organization := class organization
]

{ #category : #running }
ProtocolAnnouncementsTest >> tearDown [

	class package removeFromSystem.
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
