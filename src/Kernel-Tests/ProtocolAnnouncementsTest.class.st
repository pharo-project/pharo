"
I am a test case specialized in checking that the announcements linked to protocol management are right.
"
Class {
	#name : 'ProtocolAnnouncementsTest',
	#superclass : 'AbstractClassDescriptionAnnouncementTest',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests - adding' }
ProtocolAnnouncementsTest >> testAddProtocolAnnouncement [

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #king.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class addProtocol: #king.
	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - adding' }
ProtocolAnnouncementsTest >> testAddProtocolAnnouncement2 [

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #king.
		self assert: ann protocol owningClass equals: class.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class addProtocol: (Protocol named: #king).
	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - adding' }
ProtocolAnnouncementsTest >> testAddProtocolAnnouncementDuringCompilation [

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class compile: 'king ^1' classified: #titan.
	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests' }
ProtocolAnnouncementsTest >> testClassifyUnderAnnounceNewProtocol [

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class classify: #king under: #titan.
	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolAnnouncement [

	class addProtocol: #titan.

	self when: ProtocolRemoved do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class removeProtocolIfEmpty: #titan.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolAnnouncement2 [

	class addProtocol: #titan.

	self when: ProtocolRemoved do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class removeProtocolIfEmpty: (class protocolNamed: #titan).

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolAnnouncement3 [

	class addProtocol: #titan.
	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self when: ProtocolRemoved do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class removeProtocol: #titan.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolAnnouncement4 [

	class addProtocol: #titan.

	self when: ProtocolRemoved do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class removeProtocol: #titan.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolAnnouncementDuringCompilation [

	class compile: 'king ^1' classified: #titan.

	self when: ProtocolRemoved do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class compile: 'king ^1' classified: #demon.

	self assert: numberOfAnnouncements equals: 1
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolIfEmptyWithNonEmptyProtocolDoesNotAnnounceAnything [

	class addProtocol: #titan.
	class compiler
		protocol: #titan;
		install: 'king ^ 1'.

	self when: ProtocolRemoved do: [ :ann | self fail ].

	class removeProtocolIfEmpty: #titan.

	self assert: numberOfAnnouncements equals: 0
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolIfEmptyWithNonExistingProtocolDoesNotAnnounceAnything [

	self when: ProtocolRemoved do: [ :ann | self fail ].

	class removeProtocolIfEmpty: #nonexisting.

	self assert: numberOfAnnouncements equals: 0
]

{ #category : 'tests - removing' }
ProtocolAnnouncementsTest >> testRemoveProtocolIfEmptyWithNonExistingProtocolDoesNotAnnounceAnything2 [

	self when: ProtocolRemoved do: [ :ann | self fail ].

	class removeProtocolIfEmpty: (Protocol named: #nonexisting).

	self assert: numberOfAnnouncements equals: 0
]

{ #category : 'tests' }
ProtocolAnnouncementsTest >> testRenameProtocolAsAnnounceNewProtocol [

	class classify: #king under: #demon.

	self when: ProtocolAdded do: [ :ann |
		self assert: ann protocol name equals: #titan.
		self assert: ann classReorganized name equals: self classNameForTests ].

	class renameProtocol: #demon as: #titan.

	self assert: numberOfAnnouncements equals: 1
]
