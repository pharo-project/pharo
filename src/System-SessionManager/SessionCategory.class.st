"
A session category allows to define many categories for a session (eg. System, Network, UI and user categories).
It allows session handlers (e.g. classes or packages handlers) to be registered into a category.
By default, the same default priority is assigned to handlers but a specific priority could be specified so that an handler will be activated before/after others.
"
Class {
	#name : #SessionCategory,
	#superclass : #Object,
	#instVars : [
		'name',
		'priorityLists'
	],
	#category : 'System-SessionManager'
}

{ #category : #accessing }
SessionCategory >> defaultPriority [
	"Completely arbitrary by now"
	^ 100
]

{ #category : #initialization }
SessionCategory >> initialize [
	super initialize.
	priorityLists := Dictionary new
]

{ #category : #private }
SessionCategory >> listAtPriority: anInteger [ 
	^ priorityLists
		at: anInteger
		ifAbsentPut: [ OrderedCollection new ]
]

{ #category : #accessing }
SessionCategory >> name [
	^ name
]

{ #category : #accessing }
SessionCategory >> name: aString [ 
	name := aString
]

{ #category : #printing }
SessionCategory >> printOn: aStream [ 
	super printOn: aStream.
	aStream nextPut: $(;
		nextPutAll: self name;
		nextPut: $)
]

{ #category : #accessing }
SessionCategory >> prioritizedList [
	^ priorityLists keys sorted 
		flatCollect: [ :index | priorityLists at: index ]
]

{ #category : #registration }
SessionCategory >> register: anObject [ 
	self
		register: anObject
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionCategory >> register: anObject atPriority: anInteger [ 
	(self listAtPriority: anInteger)
		addLast: anObject
]

{ #category : #private }
SessionCategory >> renamedClass: aClass from: oldName to: newName [
	
	priorityLists do: [ :handlers |
		handlers 
			detect: [ :handler | handler handledId = oldName ]
			ifFound: [ :handler | handler registeredClassName: newName ] ]

]

{ #category : #registration }
SessionCategory >> unregisterHandler: anHandlerId [ 
	priorityLists values
		do: [ :handlers |
				handlers 
					detect: [ :handler | handler handledId = anHandlerId ]
					ifFound: [ :handler | handlers remove: handler ] ]
]
