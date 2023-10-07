Class {
	#name : 'SystemAnnouncerTest',
	#superclass : 'TestCase',
	#instVars : [
		'oldSystemAnnouncer',
		'factory'
	],
	#category : 'System-Announcements-Tests',
	#package : 'System-Announcements-Tests'
}

{ #category : 'running' }
SystemAnnouncerTest >> setUp [
	super setUp.

	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.

	factory := ClassFactoryForTestCase new
]

{ #category : 'running' }
SystemAnnouncerTest >> tearDown [

	SystemAnnouncer announcer: oldSystemAnnouncer.
	factory cleanUp.
	super tearDown
]

{ #category : 'tests' }
SystemAnnouncerTest >> testProtocolAdded [

	| pass class classReorganized protocolAdded |
	pass := false.

	SystemAnnouncer uniqueInstance
		when: ProtocolAdded do: [ :ann |
			pass := true.
			classReorganized := ann classReorganized.
			protocolAdded := ann protocol ]
		for: self.

	class := factory newClass.
	class addProtocol: 'shiny-new-category'.

	self assert: pass.
	self assert: classReorganized equals: class.
	self assert: protocolAdded name equals: 'shiny-new-category'
]

{ #category : 'tests' }
SystemAnnouncerTest >> testProtocolRemoved [

	| pass class classRemoved protocolRemoved |
	pass := false.

	SystemAnnouncer uniqueInstance
		when: ProtocolRemoved do: [ :ann |
			pass := true.
			classRemoved := ann classReorganized.
			protocolRemoved := ann protocol ]
		for: self.

	class := factory newClass.
	class addProtocol: 'shiny-new-category'.

	class removeProtocolIfEmpty: 'shiny-new-category'.

	self assert: pass.
	self assert: classRemoved equals: class.
	self assert: protocolRemoved name equals: 'shiny-new-category'
]

{ #category : 'tests' }
SystemAnnouncerTest >> testSuspendAllWhile [
	| value |

	value := 42.

	SystemAnnouncer uniqueInstance suspendAllWhile: [
		SystemAnnouncer uniqueInstance
			when: ClassAdded do: [ value := value + 1 ] for: self;
			announce: ClassAdded ].

	self assert: value equals: 42. "The answer is always 42 :)"

	SystemAnnouncer announce: ClassAdded.

	self assert: value equals: 43
]

{ #category : 'tests' }
SystemAnnouncerTest >> testSuspendAllWhileStoring [

	| stored |

	stored := SystemAnnouncer uniqueInstance suspendAllWhileStoring: [ SystemAnnouncer announce: ClassAdded ].

	self assert: stored size equals: 1.
	self assert: ( stored at: 1 ) equals: ClassAdded
]

{ #category : 'tests' }
SystemAnnouncerTest >> testSuspendAllWhileStoringNested [
	|  stored |

	stored := SystemAnnouncer uniqueInstance suspendAllWhileStoring: [
		SystemAnnouncer uniqueInstance suspendAllWhileStoring:[SystemAnnouncer announce: ClassAdded ]
	].

	self assert: stored size equals: 1.
	self assert: (stored at:1) equals: ClassAdded.
]
