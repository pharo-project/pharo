"
I allow one to group together commands that are semantically related.

I can hold either commands or sub-groups.

I provide a nice API to query the tree of commands and groups. See my 'public-api' protocol.
"
Class {
	#name : #CmCommandGroup,
	#superclass : #CmAbstractCommandsGroup,
	#traits : 'CmTWithBasicNameAndDescription + CmTDecorable',
	#classTraits : 'CmTWithBasicNameAndDescription classTrait + CmTDecorable classTrait',
	#instVars : [
		'entries'
	],
	#category : #'Commander2-Groups'
}

{ #category : #default }
CmCommandGroup class >> defaultDescription [
	^ 'I''m group and I should provide a nice description for my user.'
]

{ #category : #default }
CmCommandGroup class >> defaultName [
	^ 'Unnamed group'
]

{ #category : #'instance creation' }
CmCommandGroup class >> forSpec [
	^ self new asSpecGroup
]

{ #category : #'instance creation' }
CmCommandGroup class >> forSpecWithIconNamed: aSymbol [
	^ self new asSpecGroupWithIconNamed: aSymbol
]

{ #category : #'instance creation' }
CmCommandGroup class >> named: aString [
	^ self new
		basicName: aString;
		yourself
]

{ #category : #accessing }
CmCommandGroup >> allCommands [
	| visitor |
	visitor := CmCommandsCollector new.
	self acceptVisitor: visitor.
	^ visitor commandsCollected
]

{ #category : #'public-api' }
CmCommandGroup >> commandOrGroupNamed: aString [
	^ (entries detect: [ :commandOrRegister | commandOrRegister basicName = aString ])
]

{ #category : #'public-api' }
CmCommandGroup >> commands [
	^ entries select: [ :entry | entry isCommand ]
]

{ #category : #'public-api' }
CmCommandGroup >> entries [
	^ entries
]

{ #category : #'public-api' }
CmCommandGroup >> groups [
	^ entries select: [ :entry | entry isGroup ]
]

{ #category : #testing }
CmCommandGroup >> hasCommand: aCmCommand [
	^ self commands includes: aCmCommand
]

{ #category : #testing }
CmCommandGroup >> hasEntryNamed: aString [
	"Returns true if one of my entries (command, registry, ...) is named aString.
	 Else returns false.
	"
	^ entries anySatisfy: [ :any | any basicName = aString ]
]

{ #category : #initialization }
CmCommandGroup >> initialize [
	super initialize.
	self basicName: self class defaultName.
	self basicDescription: self class defaultDescription.
	entries := OrderedCollection new
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCmCommandsOrGroup [
	(self hasEntryNamed: aCmCommandsOrGroup basicName)
		ifTrue: [ CmDuplicatedEntryName signalEntryNamed: aCmCommandsOrGroup basicName ].
	
	entries add: aCmCommandsOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCmCommandsOrGroup after: anotherLtCommandsOrGroup [
	self shouldBeImplemented; flag: #TODO
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCmCommandsOrGroup before: anotherLtCommandsOrGroup [
	self shouldBeImplemented; flag: #TODO
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCmCommandsOrGroup insteadOf: anotherLtCommandsOrGroup [
	self shouldBeImplemented; flag: #TODO
]

{ #category : #'public-api' }
CmCommandGroup >> registerFirst: aCmCommandsOrGroup [
	self shouldBeImplemented; flag: #TODO
]

{ #category : #'public-api' }
CmCommandGroup >> registerLast: aCmCommandsOrGroup [
	self shouldBeImplemented; flag: #TODO
]

{ #category : #'public-api' }
CmCommandGroup >> unregister: aCmCommandOrGroup [
	entries remove: (entries detect: [ :e | e = aCmCommandOrGroup ])
]
