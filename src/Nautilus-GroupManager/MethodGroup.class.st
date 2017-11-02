"
A group containing methods
"
Class {
	#name : #MethodGroup,
	#superclass : #Group,
	#instVars : [
		'methods'
	],
	#category : 'Nautilus-GroupManager'
}

{ #category : #'instance creation' }
MethodGroup class >> named: aString methods: aCollectionOfMethods [
	^ self new
		name: aString;
		methods: aCollectionOfMethods;
		yourself
]

{ #category : #queries }
MethodGroup >> classes [
	| classesCollection |
	classesCollection := OrderedCollection new.
	methods
		do: [ :method | method methodClass ifNotNil: [ classesCollection addIfNotPresent:  method methodClass theNonMetaClass ] ].
	^ classesCollection
]

{ #category : #accessing }
MethodGroup >> elements [
	^ methods
]

{ #category : #testing }
MethodGroup >> hasClassNamed: aClassName [
	^ self classes includes: aClassName
]

{ #category : #initialization }
MethodGroup >> initialize [
	super initialize.
	methods := OrderedCollection new.
	SystemAnnouncer uniqueInstance weak when: MethodAdded send: #methodAdded: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #methodModified: to: self
]

{ #category : #announcements }
MethodGroup >> methodAdded: anAnnouncement [
	methods add: anAnnouncement methodAdded
]

{ #category : #announcements }
MethodGroup >> methodModified: anAnnouncement [
	methods add: anAnnouncement newMethod
]

{ #category : #accessing }
MethodGroup >> methods [
	^ methods
]

{ #category : #accessing }
MethodGroup >> methods: aCollection [
	methods := aCollection.
]

{ #category : #queries }
MethodGroup >> methodsFor: aClass [
	| methodsForAClass |
	methodsForAClass := OrderedCollection new.
	aClass methods
		do: [ :method | 
			method methodClass = aClass
				ifTrue: [ methodsForAClass add: method ] ].
	^ methodsForAClass
]

{ #category : #queries }
MethodGroup >> methodsFor: aClass categorised: aSymbol [
	| methodsForAClass |
	aSymbol ifNil: [ ^ self methodsFor: aClass ].
	methodsForAClass := OrderedCollection new.
	( aClass methodsInProtocol: aSymbol)
		do: [ :method | 
			method methodClass = aClass
				ifTrue: [ methodsForAClass add: method ] ].
	^ methodsForAClass
]

{ #category : #queries }
MethodGroup >> protocolsFor: aClass [

	^ aClass protocols sort
]
