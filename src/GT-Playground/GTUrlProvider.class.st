"
This class was inspired from the Zinc SharedWorkspace project.

Its job is to work with a UrlProvider that can provide (via get) and store (via post) strings.
"
Class {
	#name : #GTUrlProvider,
	#superclass : #Object,
	#instVars : [
		'urlProvider'
	],
	#classInstVars : [
		'urlProvider'
	],
	#category : #'GT-Playground'
}

{ #category : #settings }
GTUrlProvider class >> settingsOn: aBuilder [ 
	<systemsettings>
	
	(aBuilder setting: #stringUrlProvider)
		parent: #gt;
		target: self;
		description: 'The url for the script sharing service like the one from http://ws.stfx.eu';
		label: 'The url for the script sharing service'

]

{ #category : #accessing }
GTUrlProvider class >> stringUrlProvider [
	^ self urlProvider asString
]

{ #category : #accessing }
GTUrlProvider class >> stringUrlProvider: aString [
	self urlProvider: aString
]

{ #category : #accessing }
GTUrlProvider class >> urlProvider [
	^ urlProvider ifNil: [ urlProvider := 'http://ws.stfx.eu' asZnUrl ]
]

{ #category : #accessing }
GTUrlProvider class >> urlProvider: aUrlObject [
	urlProvider := aUrlObject ifNotNil: [ aUrlObject asZnUrl ]
]

{ #category : #public }
GTUrlProvider >> get: url [	
	| contents |
	contents := self httpClient get: url.
	^ String new: contents size streamContents: [ :stream |
		contents linesDo: [ :each | stream nextPutAll: each; cr ] ]
]

{ #category : #private }
GTUrlProvider >> httpClient [
	^ ZnClient new
		systemPolicy;
		beOneShot;
		accept: ZnMimeType textPlain;
		url: self urlProvider;
		yourself
]

{ #category : #public }
GTUrlProvider >> post: string [
	| entity client url |
	entity := ZnEntity text: string asString.
	(client := self httpClient)
		entity: entity; 
		post.
	url := client response location.
	^ url asZnUrl inContextOf: self urlProvider 
]

{ #category : #public }
GTUrlProvider >> urlProvider [
	^ urlProvider ifNil: [ urlProvider := self class urlProvider ]
]

{ #category : #'initialize-release' }
GTUrlProvider >> urlProvider: anUrlObject [
	urlProvider := anUrlObject asZnUrl 
]
