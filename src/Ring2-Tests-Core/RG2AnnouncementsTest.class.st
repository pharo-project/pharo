Class {
	#name : #RG2AnnouncementsTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2AnnouncementsTest >> testAnnouncementTimestamps [

	| env announcements behavior start end |
	
	start := DateAndTime now.
	env := RG2Environment new.
	announcements := OrderedCollection new.
	env announcer when: ClassAdded do: [ :announcement | announcements add: announcement ].
	
	env ensureClassNamed: #SomeClass1.
	10 milliSeconds wait.
	env ensureClassNamed: #SomeClass2.

	end := DateAndTime now.

	self assert: (announcements size >=2).
	self assert: (announcements allSatisfy: [:each | each timeStamp between: start and: end] ).
	self assert: (announcements first timeStamp < announcements last timeStamp).
		
	"try setters"
	announcements first timeStamp: end.
	self assert: announcements first timeStamp equals: end.
]

{ #category : #tests }
RG2AnnouncementsTest >> testAnnouncerSuspending [

	| env announcements behavior | 
	
	env := RG2Environment new.
	announcements := OrderedCollection new.
	env announcer when: ClassAdded do: [ :announcement | announcements add: announcement ].
	
	env announcer suspendAllWhile: [ 
		behavior := env ensureClassNamed: #SomeClass.].
	
	self assert: announcements isEmpty
]

{ #category : #tests }
RG2AnnouncementsTest >> testAnnouncerSuspendingWithStoring [

	| env announcements behavior stored | 
	
	env := RG2Environment new.
	announcements := OrderedCollection new.
	env announcer when: ClassAdded do: [ :announcement | announcements add: announcement ].
	
	stored := env announcer suspendAllWhileStoring: [ 
		behavior := env ensureClassNamed: #SomeClass.].
	
	self assert: announcements isEmpty.
	self assert: stored notEmpty.
	"TODO: more detailed check"
]

{ #category : #tests }
RG2AnnouncementsTest >> testBehaviorAdded [

	| env announcements behavior | 
	
	env := RG2Environment new.
	announcements := OrderedCollection new.
	env announcer when: ClassAdded do: [ :announcement | announcements add: announcement ].
	
	behavior := env ensureClassNamed: #SomeClass.
	self assert: announcements size equals: 4.
	self assert: announcements first classAffected == behavior.
	
	self assert: (announcements select: [:each | each classAffected isRing2Resolved]) size equals: 1.
	
	
]

{ #category : #tests }
RG2AnnouncementsTest >> testBehaviorCommentModified [

	| env announcements behavior | 
	
	env := RG2Environment new.
	announcements := OrderedCollection new.
	env announcer when: ClassCommented do: [ :announcement | announcements add: announcement ].
	
	behavior := env ensureClassNamed: #SomeClass.
	self assert: announcements isEmpty.
	behavior comment content: 'some comment'.
	self assert: announcements size equals: 1.
	behavior comment author: 'SomeAuthor'.
	behavior comment time: DateAndTime new.
	self assert: announcements size equals: 3.
	self assert: (announcements allSatisfy: [:each | each classCommented == behavior ]).
	
	
	
	
	
]

{ #category : #tests }
RG2AnnouncementsTest >> testBehaviorDefinitionModifiedForCategory [

	| env announcements class | 
	
	env := RG2Environment new.
	class := env ensureClassNamed: #SomeClass.
	
	announcements := OrderedCollection new.
	env announcer when: ClassModifiedClassDefinition do: [ :announcement |
		(announcement classAffected == class) ifTrue: [ 
			announcements add: announcement]].
	
	class category: 'some category'.
	self assert: announcements size equals: 1.
	
	
	
	
]

{ #category : #tests }
RG2AnnouncementsTest >> testBehaviorDefinitionModifiedForSuperclass [

	| env announcements class superclass1 superclass2 | 
	
	env := RG2Environment new.
	class := env ensureClassNamed: #SomeClass.
	superclass1 := env ensureClassNamed: #SomeSuperclass1.
	superclass2 := env ensureClassNamed: #SomeSuperclass2.
	
	class superclass: superclass1.
	
	announcements := OrderedCollection new.
	env announcer when: ClassModifiedClassDefinition do: [ :announcement |
		(announcement classAffected == class) ifTrue: [ 
			announcements add: announcement]].
	
	class name: #NewName.
	self assert: announcements size equals: 1.
	superclass1 name: #NewSuperclass1.
	self assert: announcements size equals: 2.
	class superclass: superclass2.
	self assert: announcements size equals: 3.
	
	
	
	
	
]

{ #category : #tests }
RG2AnnouncementsTest >> testBehaviorParentRenamed [

	| env announcements class superclass | 
	
	env := RG2Environment new.
	class := env ensureClassNamed: #SomeClass.
	superclass := env ensureClassNamed: #SomeSuperclass.
	
	class superclass: superclass.
	
	announcements := OrderedCollection new.
	env announcer when: ClassParentRenamed do: [ :announcement |
		(announcement classAffected == class) ifTrue: [ 
			announcements add: announcement]].
	
	class name: #NewName.
	self assert: announcements size equals: 0.
	superclass name: #NewSuperclass1.
	self assert: announcements size equals: 1.
	
	
	
	
	
]

{ #category : #tests }
RG2AnnouncementsTest >> testDirectAnnouncement [

	| def announcements  |
	
	def := RG2BehaviorDefinition new.
	announcements := OrderedCollection new.
	def environment announcer when: ClassAdded do: [ :announcement | announcements add: announcement ].
	def announce: (ClassAdded class: def category: nil).
	
	self assert: announcements size equals: 1.
	self assert: (announcements first classAffected == def).
]

{ #category : #tests }
RG2AnnouncementsTest >> testUnsubscribe [

	| env announcements behavior | 
	
	env := RG2Environment new.
	announcements := OrderedCollection new.
	env announcer when: ClassAdded do: [ :announcement | announcements add: announcement ].
	env announcer unsubscribe: self.
	
	behavior := env ensureClassNamed: #SomeClass.
	self assert: announcements isEmpty.

]
