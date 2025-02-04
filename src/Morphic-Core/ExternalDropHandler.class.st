"
Handle a number of dropped files from the OS
"
Class {
	#name : 'ExternalDropHandler',
	#superclass : 'Object',
	#instVars : [
		'action',
		'type',
		'extension'
	],
	#classVars : [
		'DefaultHandler',
		'RegisteredHandlers'
	],
	#category : 'Morphic-Core-Worlds',
	#package : 'Morphic-Core',
	#tag : 'Worlds'
}

{ #category : 'private' }
ExternalDropHandler class >> chooseServiceFrom: aCollection [
	"private - choose a service from aCollection asking the user if
	needed"

	^ self morphicUIManager
		  chooseFrom: (aCollection collect: [ :each | each label ])
		  values: aCollection
]

{ #category : 'cleanup' }
ExternalDropHandler class >> cleanUp [
	self resetRegisteredHandlers
]

{ #category : 'accessing' }
ExternalDropHandler class >> defaultHandler [
	DefaultHandler ifNil: [DefaultHandler := ExternalDropHandler type: nil extension: nil action: [:dropStream | dropStream edit]].
	^DefaultHandler
]

{ #category : 'accessing' }
ExternalDropHandler class >> defaultHandler: externalDropHandler [
	DefaultHandler := externalDropHandler
]

{ #category : 'class initialization' }
ExternalDropHandler class >> initialize [

	self resetRegisteredHandlers
]

{ #category : 'accessing' }
ExternalDropHandler class >> lookupExternalDropHandler: aFileReference [

	| types |
	types := aFileReference mimeTypes.

	types ifNotNil: [
		self registeredHandlers do: [ :handler |
			(handler matchesTypes: types) ifTrue: [ ^ handler ] ] ].

	self registeredHandlers do: [ :handler |
		(handler matchesExtension: aFileReference extension) ifTrue: [
			^ handler ] ].
	^ self lookupServiceBasedHandler: aFileReference
]

{ #category : 'accessing' }
ExternalDropHandler class >> lookupServiceBasedHandler: aFileReference [
	"Choose the service handler from those installed, and if none, present a default one.
	Let the user choose.
	
	If the user cancels, return nil.
	Otherwise, return the user's choice"

	| services theOne |
	services := (FileServices itemsForFile: aFileReference fullName)
		reject: [:svc | self unwantedSelectors includes: svc selector].
	services := services copyWith: self defaultHandler.

	theOne := self chooseServiceFrom: services.
	^ theOne ifNotNil: [
		  self
			  type: nil
			  extension: nil
			  action: [ :stream | theOne performServiceFor: aFileReference ] ]
]

{ #category : 'accessing' }
ExternalDropHandler class >> registerHandler: aHandler [
	self registeredHandlers add: aHandler
]

{ #category : 'private' }
ExternalDropHandler class >> registeredHandlers [
	RegisteredHandlers ifNil: [RegisteredHandlers := OrderedCollection new].
	^RegisteredHandlers
]

{ #category : 'private' }
ExternalDropHandler class >> resetRegisteredHandlers [
	RegisteredHandlers := nil
]

{ #category : 'instance creation' }
ExternalDropHandler class >> type: aType extension: anExtension action: anAction [
	^self new type: aType extension: anExtension action: anAction
]

{ #category : 'private' }
ExternalDropHandler class >> unwantedSelectors [
	"private - answer a collection well known unwanted selectors "
	^ #(#removeLineFeeds: #compressFile:)
]

{ #category : 'accessing' }
ExternalDropHandler >> extension [
	^extension
]

{ #category : 'accessing' }
ExternalDropHandler >> handle: dropStream in: pasteUp dropEvent: anEvent [

	^action cull: dropStream
			cull: pasteUp
			cull: anEvent
]

{ #category : 'testing' }
ExternalDropHandler >> matchesExtension: aExtension [
	(self extension isNil or: [ aExtension isNil ]) ifTrue: [ ^ false ].
	^ FileSystem disk isCaseSensitive
		ifTrue: [ extension = aExtension ]
		ifFalse: [ extension sameAs: aExtension ]
]

{ #category : 'testing' }
ExternalDropHandler >> matchesTypes: types [
	(self type isNil or: [types isNil])
		ifTrue: [^false].
	^types anySatisfy: [:mimeType | mimeType main beginsWith: self type]
]

{ #category : 'accessing' }
ExternalDropHandler >> type [
	^type
]

{ #category : 'initialize' }
ExternalDropHandler >> type: aType extension: anExtension action: anAction [
	action := anAction.
	type := aType.
	extension := anExtension
]
