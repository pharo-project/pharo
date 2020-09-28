"
I am a singleton object, subscribed to system events, to listen to the creation of methods marked with the <keymap> and keymap:> pragmas.

When I listen one of those events, I reinitialize the KMRepository default instance and reload it with all declared keymaps.
"
Class {
	#name : #KMPragmaKeymapBuilder,
	#superclass : #Object,
	#instVars : [
		'model'
	],
	#classVars : [
		'UniqueInstace'
	],
	#category : #'Keymapping-Pragmas'
}

{ #category : #'instance creation' }
KMPragmaKeymapBuilder class >> initialize [
	<script>
	self uniqueInstance reset
]

{ #category : #private }
KMPragmaKeymapBuilder class >> methodAnnouncementReceived: anAnnouncement [
	| method |
	method := anAnnouncement methodAffected.
	method ifNil: [ ^self ].
	method hasPragma ifFalse: [ ^self ].
	(method pragmas anySatisfy: [:pragma | self pragmas includes: pragma selector] )
		ifTrue: [ self uniqueInstance reset ]
]

{ #category : #'instance creation' }
KMPragmaKeymapBuilder class >> pragmas [
	^#(#keymap #keymap:)
]

{ #category : #'system-events' }
KMPragmaKeymapBuilder class >> registerInterestToSystemAnnouncement [
	<systemEventRegistration>
	SystemAnnouncer uniqueInstance 
		unsubscribe: self.
	SystemAnnouncer uniqueInstance weak 
		when: MethodAdded, MethodModified, MethodRemoved 
		send: #methodAnnouncementReceived: 
		to: self.
]

{ #category : #'instance creation' }
KMPragmaKeymapBuilder class >> release [
	SystemAnnouncer uniqueInstance unsubscribe: self.
	self uniqueInstance release
]

{ #category : #'instance creation' }
KMPragmaKeymapBuilder class >> uniqueInstance [
	^UniqueInstace ifNil: [ UniqueInstace := self new ]
]

{ #category : #accessing }
KMPragmaKeymapBuilder >> builder [
	^ self
]

{ #category : #'registrations handling' }
KMPragmaKeymapBuilder >> collectRegistrations [
	| menu |
	menu := PragmaMenuAndShortcutRegistration model: self.
	self pragmas
		do: [ :prg | 
			prg methodClass instanceSide
				perform: prg methodSelector
				with:
					(menu
						platform: prg arguments;
						yourself) ].
	self interpretRegistration: menu
]

{ #category : #private }
KMPragmaKeymapBuilder >> interpretRegistration: aRegistration [
	
	aRegistration handOutItems do: [:item || node |
		node := KMKeymapBuilder 
					for: item shortcut
					platform: item platform.
		item help 
			ifNil: [
				node
					category: item category
					shortcut: item default
					do: item action ]
			ifNotNil:[ 
				node
					category: item category
					shortcut: item default
					do: item action
					description: item help ]]
]

{ #category : #accessing }
KMPragmaKeymapBuilder >> itemReceiver [
	^ model
]

{ #category : #accessing }
KMPragmaKeymapBuilder >> model [
	^ model
]

{ #category : #accessing }
KMPragmaKeymapBuilder >> model: anObject [
	model := anObject
]

{ #category : #accessing }
KMPragmaKeymapBuilder >> pragmaKeywords [
	"Returns the pragmas keyword used to select pragmas"
	^  self class pragmas
]

{ #category : #'registrations handling' }
KMPragmaKeymapBuilder >> pragmas [

	"Return all pragmas which keyword is self pragmaKeyword"

	^ (self pragmaKeywords flatCollect: [ :each | Pragma allNamed: each ]) 
		  select: [ :prg | prg methodSelector numArgs = 1 ]
]

{ #category : #dependencies }
KMPragmaKeymapBuilder >> release [
	model := nil.
	super release.
	
]

{ #category : #initialization }
KMPragmaKeymapBuilder >> reset [
	self collectRegistrations
]
