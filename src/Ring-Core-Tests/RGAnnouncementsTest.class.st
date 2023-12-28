Class {
	#name : 'RGAnnouncementsTest',
	#superclass : 'RGTest',
	#category : 'Ring-Core-Tests',
	#package : 'Ring-Core-Tests'
}

{ #category : 'announcements' }
RGAnnouncementsTest >> installAnnouncementsFor: aRGClass in: aRGEnvironment when: anEvent [

	| announcements |
	announcements := OrderedCollection new.
	aRGEnvironment announcer
		when: anEvent
		do: [ :announcement |
			announcement classAffected == aRGClass ifTrue: [
				announcements add: announcement ] ]
		for: self.
	^ announcements
]

{ #category : 'announcements' }
RGAnnouncementsTest >> installAnnouncementsIn: aRGEnvironment when: anEvent [

	| announcements |
	announcements := OrderedCollection new.
	aRGEnvironment announcer
		when: anEvent
		do: [ :announcement | announcements add: announcement ]
		for: self.
	^ announcements
]

{ #category : 'tests' }
RGAnnouncementsTest >> testAnnouncementTimestamps [

	| env announcements start end |

	start := DateAndTime now.
	env := RGEnvironment new.
	announcements := self installAnnouncementsIn: env when: ClassAdded. 

	env ensureClassNamed: #SomeClass1.
	10 milliSeconds wait.
	env ensureClassNamed: #SomeClass2.

	end := DateAndTime now.

	self assert: (announcements size >=2).
	self assert: (announcements allSatisfy: [:each | each timeStamp between: start and: end] ).
	self assert: (announcements first timeStamp < announcements last timeStamp).

	"try setters"
	announcements first timeStamp: end.
	self assert: announcements first timeStamp equals: end
]

{ #category : 'tests' }
RGAnnouncementsTest >> testAnnouncerSuspending [

	| env announcements behavior |

	env := RGEnvironment new.
	announcements := self installAnnouncementsIn: env when: ClassAdded. 

	env announcer suspendAllWhile: [
		behavior := env ensureClassNamed: #SomeClass.].

	self assert: announcements isEmpty
]

{ #category : 'tests' }
RGAnnouncementsTest >> testAnnouncerSuspendingWithStoring [

	| env announcements behavior stored |

	env := RGEnvironment new.
	announcements := self installAnnouncementsIn: env when: ClassAdded. 

	stored := env announcer suspendAllWhileStoring: [
		behavior := env ensureClassNamed: #SomeClass.].

	self assert: announcements isEmpty.
	self assert: stored notEmpty.
	"TODO: more detailed check"
]

{ #category : 'tests' }
RGAnnouncementsTest >> testBehaviorAdded [
	| env announcements behavior |
	env := RGEnvironment new.
	announcements := self installAnnouncementsIn: env when: ClassAdded. 

	behavior := env ensureClassNamed: #SomeClass.
	self assert: announcements size equals: 4.
	self assert: announcements first classAffected identicalTo: behavior.

	self assert: (announcements select: [ :each | each classAffected isRingResolved ]) size equals: 1
]

{ #category : 'tests' }
RGAnnouncementsTest >> testBehaviorCommentModified [

	| env announcements behavior |

	env := RGEnvironment new.
	announcements := self installAnnouncementsIn: env when: ClassCommented. 

	behavior := env ensureClassNamed: #SomeClass.
	self assert: announcements isEmpty.
	behavior comment content: 'some comment'.
	self assert: announcements size equals: 1.
	behavior comment author: 'SomeAuthor'.
	behavior comment time: DateAndTime new.
	self assert: announcements size equals: 3.
	self assert: (announcements allSatisfy: [:each | each classCommented == behavior ])
]

{ #category : 'tests' }
RGAnnouncementsTest >> testBehaviorDefinitionModifiedForCategory [

	| env announcements class |

	env := RGEnvironment new.
	class := env ensureClassNamed: #SomeClass.

	announcements := self installAnnouncementsFor: class in: env when: ClassModifiedClassDefinition. 

	class category: 'some category'.
	self assert: announcements size equals: 1
]

{ #category : 'tests' }
RGAnnouncementsTest >> testBehaviorDefinitionModifiedForSuperclass [

	| env announcements class superclass1 superclass2 |

	env := RGEnvironment new.
	class := env ensureClassNamed: #SomeClass.
	superclass1 := env ensureClassNamed: #SomeSuperclass1.
	superclass2 := env ensureClassNamed: #SomeSuperclass2.

	class superclass: superclass1.

	announcements := self installAnnouncementsFor: class in: env when: ClassModifiedClassDefinition.  

	class name: #NewName.
	self assert: announcements size equals: 1.
	superclass1 name: #NewSuperclass1.
	self assert: announcements size equals: 2.
	class superclass: superclass2.
	self assert: announcements size equals: 3
]

{ #category : 'tests' }
RGAnnouncementsTest >> testBehaviorParentRenamed [

	| env announcements class superclass |

	env := RGEnvironment new.
	class := env ensureClassNamed: #SomeClass.
	superclass := env ensureClassNamed: #SomeSuperclass.

	class superclass: superclass.

	announcements := self installAnnouncementsFor: class in: env when: ClassParentRenamed. 

	class name: #NewName.
	self assert: announcements size equals: 0.
	superclass name: #NewSuperclass1.
	self assert: announcements size equals: 1
]

{ #category : 'tests' }
RGAnnouncementsTest >> testDirectAnnouncement [

	| def announcements |
	def := RGBehavior new.
	announcements := self installAnnouncementsIn: def when: ClassAdded.
	def announce: (ClassAdded class: def).

	self assert: announcements size equals: 1.
	self assert: announcements first classAffected identicalTo: def
]

{ #category : 'tests' }
RGAnnouncementsTest >> testUnsubscribe [

	| env announcements behavior |

	env := RGEnvironment new.
	announcements := self installAnnouncementsIn: env when: ClassAdded. 
	env announcer unsubscribe: self.

	behavior := env ensureClassNamed: #SomeClass.
	self assert: announcements isEmpty
]
