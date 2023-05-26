"
This class manages the class comment and a protocol organizer
"
Class {
	#name : #ClassOrganization,
	#superclass : #Object,
	#instVars : [
		'organizedClass',
		'protocols'
	],
	#category : #'Kernel-Protocols'
}

{ #category : #'instance creation' }
ClassOrganization class >> forClass: aClass [
	^ self basicNew
		initializeClass: aClass;
		yourself
]

{ #category : #accessing }
ClassOrganization >> addProtocol: aProtocol [

	| oldProtocols protocol |
	
	(self organizedClass hasProtocol: aProtocol) ifTrue: [
		^ self organizedClass protocolNamed: (aProtocol isString ifTrue: [ aProtocol ] ifFalse: [ aProtocol name ]) ].

	protocol := aProtocol isString ifTrue: [ Protocol named: aProtocol ] ifFalse: [ aProtocol ].

	oldProtocols := self organizedClass protocolNames copy.

	protocols := protocols copyWith: protocol.

	SystemAnnouncer announce: (ProtocolAdded in: self organizedClass protocol: protocol).
	SystemAnnouncer announce: (ClassReorganized class: self organizedClass).
	^ protocol
]

{ #category : #'backward compatibility' }
ClassOrganization >> allMethodSelectors [

	self deprecated: 'Use #selectors on the organized class' transformWith: '`@rcv allMethodSelectors' -> '`@rcv organizedClass selectors'.
	^ self organizedClass selectors
]

{ #category : #classification }
ClassOrganization >> classify: selector under: aProtocol [

	| oldProtocol newProtocol |
	(newProtocol := self ensureProtocol: aProtocol) = (oldProtocol := self protocolOfSelector: selector) ifTrue: [ ^ self ].

	oldProtocol ifNotNil: [
		oldProtocol removeMethodSelector: selector.
		self removeProtocolIfEmpty: oldProtocol ].

	newProtocol addMethodSelector: selector.

	"During the first classification of a method we dont need to announce the classification because users can subscribe to the method added announcement."
	oldProtocol ifNotNil: [ self organizedClass notifyOfRecategorizedSelector: selector from: oldProtocol to: newProtocol ]
]

{ #category : #copying }
ClassOrganization >> copyFrom: otherOrganization [

	protocols := otherOrganization protocols copy
]

{ #category : #accessing }
ClassOrganization >> ensureProtocol: aProtocol [
	"I can take a Protocol or a protocol name as paramater.
	
	If my parameter is a name, I'll return a protocol associated with it. A new one if needed.
	If my parameter is a Protocol, I'll ensure that it comes from me, else I'll throw an error.
	If I receive nil, I return the unclassified protocol"

	aProtocol ifNil: [ ^ self ensureProtocol: Protocol unclassified ].

	aProtocol isString ifFalse: [
		(self protocols includes: aProtocol)
			ifTrue: [ ^ aProtocol ]
			ifFalse: [ self error: 'I received a real protocol but this one is not part of me.' ] ].
	^ self organizedClass protocolNamed: aProtocol ifAbsent: [ self addProtocol: aProtocol ]
]

{ #category : #testing }
ClassOrganization >> includesSelector: selector [

	^ self protocols anySatisfy: [ :each | each includesSelector: selector ]
]

{ #category : #initialization }
ClassOrganization >> initialize [

	super initialize.
	self reset
]

{ #category : #initialization }
ClassOrganization >> initializeClass: aClass [

	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each | self classify: each under: Protocol unclassified ]
]

{ #category : #accessing }
ClassOrganization >> organizedClass [

	^ organizedClass ifNil: [ self error: 'ClassOrganization should always have an organized class associated.' ]
]

{ #category : #printing }
ClassOrganization >> printOn: aStream [

	super printOn: aStream.

	aStream
		nextPutAll: ' (';
		print: self organizedClass;
		nextPut: $)
]

{ #category : #inlined }
ClassOrganization >> protocolOfSelector: aSelector [

	^ self organizedClass protocolOfSelector: aSelector
]

{ #category : #accessing }
ClassOrganization >> protocols [

	^ protocols
]

{ #category : #inlined }
ClassOrganization >> removeElement: aSelector [
	self deprecated: 'Use #removeFromProtocols: from the organized class instead.' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv organizedClass removeFromProtocols: `@arg'. 
	^ self organizedClass removeFromProtocols: aSelector
]

{ #category : #inlined }
ClassOrganization >> removeProtocolIfEmpty: aProtocol [
	"I take a protocol or a protocol name and remvoe it if it is empty."

	| protocol oldProtocolNames |
	(self organizedClass hasProtocol: aProtocol) ifFalse: [ ^ self ].

	protocol := aProtocol isString ifTrue: [ self organizedClass protocolNamed: aProtocol ] ifFalse: [ aProtocol ].

	protocol isEmpty ifFalse: [ ^ self ].

	oldProtocolNames := self organizedClass protocolNames.
	protocols := protocols copyWithout: protocol.
	SystemAnnouncer announce: (ProtocolRemoved in: self organizedClass protocol: protocol).
	SystemAnnouncer announce: (ClassReorganized class: self organizedClass)
]

{ #category : #removing }
ClassOrganization >> renameProtocol: anOldProtocol as: aNewProtocol [

	| oldProtocol newProtocol |
	(aNewProtocol isNil or: [ anOldProtocol isNil ]) ifTrue: [ ^ self ].

	(self organizedClass hasProtocol: anOldProtocol) ifFalse: [ ^ self ].

	"We will announce the class reorganized later in this method. No need to duplicate the same announcement."
	SystemAnnouncer uniqueInstance prevent: ClassReorganized during: [
		oldProtocol := self ensureProtocol: anOldProtocol.

		"This method still contains a hack. We do not announce the creation of the new protocol and removal of the old protocol if the new protocol does not exist in the class yet.
	This is due to the fact that RPackage currently has a bug while listening to those events. We need to locate and fix this bug in order to do all announcements..."
		(self organizedClass hasProtocol: aNewProtocol)
			ifTrue: [
				newProtocol := self ensureProtocol: aNewProtocol.

				oldProtocol = newProtocol ifTrue: [ ^ self ].

				newProtocol addAllMethodsFrom: oldProtocol ]
			ifFalse: [ "Ideally the code in the #ifTrue: should be executed all the time and the #ifFalse: should be removed."
				| newProtocolName |
				newProtocolName := aNewProtocol isString
					                   ifTrue: [ aNewProtocol ]
					                   ifFalse: [ aNewProtocol name ].
				oldProtocol name = newProtocolName ifTrue: [ ^ self ].
				newProtocol := oldProtocol copy
					               name: newProtocolName;
					               yourself.
				protocols := protocols copyWith: newProtocol ].
		oldProtocol resetMethodSelectors.
		self removeProtocolIfEmpty: oldProtocol ].

	"Announce the changes in the system"
	SystemAnnouncer announce: (ClassReorganized class: self organizedClass).

	"I need to notify also the selector changes, otherwise RPackage will not notice"
	newProtocol methodSelectors do: [ :each | self organizedClass notifyOfRecategorizedSelector: each from: oldProtocol to: newProtocol ]
]

{ #category : #initialization }
ClassOrganization >> reset [

	protocols := Array new
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [

	organizedClass := anObject
]
