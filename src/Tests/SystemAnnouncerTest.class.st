Class {
	#name : #SystemAnnouncerTest,
	#superclass : #TestCase,
	#instVars : [
		'oldSystemAnnouncer',
		'factory'
	],
	#category : #'Tests-SystemChangeNotification'
}

{ #category : #running }
SystemAnnouncerTest >> setUp [ 
	super setUp. 
	
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer announcer: nil.
	
	factory := ClassFactoryForTestCase new.
]

{ #category : #running }
SystemAnnouncerTest >> tearDown [

	SystemAnnouncer announcer: oldSystemAnnouncer.	
	factory cleanUp.
	super tearDown.
]

{ #category : #tests }
SystemAnnouncerTest >> testProtocolAdded [
	| pass class classReorganized protocolAdded |
	
	pass := false.
	
	SystemAnnouncer uniqueInstance 
		when: ProtocolAdded do: [ :ann | 
			pass := true.
			classReorganized := ann classReorganized.
			protocolAdded := ann protocol ].
		
	class := factory newClass.
	class organization addCategory: 'shiny-new-category'.
		
	self assert: pass.
	self assert: classReorganized equals: class.
	self assert: protocolAdded equals: 'shiny-new-category'
]

{ #category : #tests }
SystemAnnouncerTest >> testProtocolRemoved [
	| pass class classRemoved protocolRemoved |
	
	pass := false.
	
	SystemAnnouncer uniqueInstance 
		when: ProtocolRemoved do: [ :ann | 
			pass := true.
			classRemoved := ann classReorganized.			
			protocolRemoved := ann protocol ].
		
	class := factory newClass.
	class organization addCategory: 'shiny-new-category'.
	
	class organization removeCategory: 'shiny-new-category'.
		
	self assert: pass.
	self assert: classRemoved equals: class.
	self assert: protocolRemoved equals: 'shiny-new-category'
]

{ #category : #tests }
SystemAnnouncerTest >> testSuspendAllWhile [
	| value |
	
	value := 42.	
	
	SystemAnnouncer uniqueInstance suspendAllWhile: [ 
		SystemAnnouncer uniqueInstance 
			when: ClassAdded do: [ value := value + 1 ];
			announce: ClassAdded ].
		
	self assert: value = 42. "The answer is always 42 :)"
	
	SystemAnnouncer uniqueInstance announce: ClassAdded.
	
	self assert: value = 43.
]

{ #category : #tests }
SystemAnnouncerTest >> testSuspendAllWhileStoring [
	| value stored |
	
	value := 42.	
	
	stored := SystemAnnouncer uniqueInstance suspendAllWhileStoring: [ SystemAnnouncer uniqueInstance announce: ClassAdded ].
		
	self assert: stored size equals: 1.
	self assert: (stored at:1) equals: ClassAdded. 
]

{ #category : #tests }
SystemAnnouncerTest >> testSuspendAllWhileStoringNested [
	| value stored |
	
	value := 42.	
	
	stored := SystemAnnouncer uniqueInstance suspendAllWhileStoring: [ 
		SystemAnnouncer uniqueInstance suspendAllWhileStoring:[SystemAnnouncer uniqueInstance announce: ClassAdded ]
	].
		
	self assert: stored size equals: 1.
	self assert: (stored at:1) equals: ClassAdded. 
]
