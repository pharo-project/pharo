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
CmCommandGroup class >> named: aString [
	^ self new
		name: aString;
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
	^ (entries detect: [ :commandOrRegister | commandOrRegister name = aString ])
]

{ #category : #'public-api' }
CmCommandGroup >> commands [
	^ entries select: [ :entry | entry isCommand ]
]

{ #category : #private }
CmCommandGroup >> ensureNotDuplicated: aCommandOrGroup [
	"Check that aCommandOrGroup's #basicName is not already used by one of my entries.
	 If it is, raises a CmDuplicatedEntryName error.
	"
	(self hasEntryNamed: aCommandOrGroup name)
		ifTrue: [ CmDuplicatedEntryName signalEntryNamed: aCommandOrGroup name ]
]

{ #category : #'public-api' }
CmCommandGroup >> entries [
	^ entries
]

{ #category : #private }
CmCommandGroup >> entriesIndexOf: aCommandOrGroup [
	"Returns the index of aCommandOrGroup in my entries.
	 If aCommandOrGroup is not included in my entries, raises a NotFound error.
	"
	^ entries
		indexOf: aCommandOrGroup
		ifAbsent:[ 
			NotFound new
				collection: self;
				object: aCommandOrGroup;
				signal: 'Command or group to replace is not found in the group.' ]
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
	^ entries anySatisfy: [ :any | any name = aString ]
]

{ #category : #testing }
CmCommandGroup >> hasGroup: aCommandGroup [
	^ self groups includes: aCommandGroup
]

{ #category : #initialization }
CmCommandGroup >> initialize [
	super initialize.
	self name: self class defaultName.
	self description: self class defaultDescription.
	entries := OrderedCollection new
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCommandOrGroup [
	self registerLast: aCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCommandOrGroup after: anotherCommandOrGroup [
	self ensureNotDuplicated: aCommandOrGroup.
	self entries add: aCommandOrGroup after: anotherCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCommandOrGroup before: anotherCommandOrGroup [
	self ensureNotDuplicated: aCommandOrGroup.
	self entries add: aCommandOrGroup before: anotherCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> register: aCommandOrGroup insteadOf: anotherCommandOrGroup [
	| commandToReplaceIndex |
	commandToReplaceIndex := self entriesIndexOf: anotherCommandOrGroup.
	
	((self commands collect: #name) \ { (entries at: commandToReplaceIndex) } includes: aCommandOrGroup name)
		ifTrue: [ CmDuplicatedEntryName signalEntryNamed: aCommandOrGroup name ].
	
	entries at: commandToReplaceIndex put: aCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> registerFirst: aCommandOrGroup [
	self ensureNotDuplicated: aCommandOrGroup.
	entries addFirst: aCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> registerLast: aCommandOrGroup [
	self ensureNotDuplicated: aCommandOrGroup.
	entries addLast: aCommandOrGroup
]

{ #category : #'public-api' }
CmCommandGroup >> unregister: aCmCommandOrGroup [
	entries remove: (entries detect: [ :e | e = aCmCommandOrGroup ])
]
