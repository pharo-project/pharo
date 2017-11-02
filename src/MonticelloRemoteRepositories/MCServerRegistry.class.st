"
Registry for metacello servers
"
Class {
	#name : #MCServerRegistry,
	#superclass : #Object,
	#instVars : [
		'registry'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'MonticelloRemoteRepositories'
}

{ #category : #'instance creation' }
MCServerRegistry class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self new ].
]

{ #category : #initialization }
MCServerRegistry >> initialize [

	super initialize.
	registry := Dictionary new.
]

{ #category : #public }
MCServerRegistry >> on: repositoryUrl beUser: nameString withPassword: passwordString [

	| credentials |
	credentials := MCServerCredentials user: nameString password: passwordString.
	registry at: repositoryUrl put: credentials.
]

{ #category : #private }
MCServerRegistry >> repositoryAt: urlString credentialsDo: aBlock [


	| possibleMatches bestMatch |
	possibleMatches := registry associations select: [ :e | urlString beginsWith: e key ].
	possibleMatches isEmpty ifTrue: [ ^ aBlock value: '' value: '' ].
	bestMatch := possibleMatches inject: possibleMatches anyOne into: [ :last :new | 
		((new key asUrl path size > last key asUrl path size) 
		"this is ugly, but URL always returns a path element, even an empty one..."
		or: [ last key asUrl path size = 1 and: [ last key asUrl path last isEmpty ]]) 
			ifTrue: [ new ]
			ifFalse: [ last ]].
	^ aBlock value: bestMatch value username value: bestMatch value password.
]
