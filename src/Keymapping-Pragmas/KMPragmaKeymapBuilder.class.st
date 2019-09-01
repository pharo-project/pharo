"
I am a singleton object, subscribed to system events, to listen to the creation of methods marked with the <keymap> and keymap:> pragmas.

When I listen one of those events, I reinitialize the KMRepository default instance and reload it with all declared keymaps.
"
Class {
	#name : #KMPragmaKeymapBuilder,
	#superclass : #Object,
	#instVars : [
		'pragmaKeywords',
		'model',
		'pragmaCollector'
	],
	#classVars : [
		'UniqueInstace'
	],
	#category : #'Keymapping-Pragmas'
}

{ #category : #private }
KMPragmaKeymapBuilder class >> event: anEvent [
	anEvent method ifNil: [ ^self ].
	((anEvent method pragmas collect: #selector) includesAnyOf: self pragmas )
		ifTrue: [ self uniqueInstance reset ]
]

{ #category : #'instance creation' }
KMPragmaKeymapBuilder class >> initialize [
	"KMPragmaKeymapBuilder initialize"
	self uniqueInstance reset.
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
		send: #event: 
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
	self pragmaCollector
		do: [ :prg | 
			prg methodClass instanceSide
				perform: prg methodSelector
				with:
					(menu
						platform: prg arguments;
						yourself) ].
	self interpretRegistration: menu
]

{ #category : #initialization }
KMPragmaKeymapBuilder >> initialize [
	super initialize.
	pragmaKeywords := OrderedCollection new.
	
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

{ #category : #'registrations handling' }
KMPragmaKeymapBuilder >> pragmaCollector [
	"Return an up-to-date pragmaCollector which contains all pragmas which keyword is self pragmaKeyword"

	^ pragmaCollector
		ifNil: [ pragmaCollector := PragmaCollector
				filter: [ :prg | 
					(self pragmaKeywords includes: prg selector)
						and: [ prg methodSelector numArgs = 1 ] ].
			(self pragmaKeywords notNil and: [ self pragmaKeywords notEmpty ])
				ifTrue: [ pragmaCollector reset ].
			pragmaCollector whenChangedSend: #reset to: self.
			pragmaCollector ]
]

{ #category : #accessing }
KMPragmaKeymapBuilder >> pragmaKeywords [
	"Returns the pragmas keyword used to select pragmas (see #pragmaCollector)"
	^  self class pragmas
]

{ #category : #initialization }
KMPragmaKeymapBuilder >> release [
	self pragmaCollector unsubscribe: self.
	self pragmaCollector announcer initialize. "Hack because the announcer is not garbage collected."
	pragmaCollector := nil.
	model := nil.
	self releaseActionMap. "we are not sure if we need it"
	super release.
	
]

{ #category : #initialization }
KMPragmaKeymapBuilder >> reset [
	pragmaCollector := nil.
	"KeymapManager default: KeymapManager new."
	self collectRegistrations.
]
