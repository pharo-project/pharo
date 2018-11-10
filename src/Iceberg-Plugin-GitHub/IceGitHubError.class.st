"
I'm an error that happens when something fails when performing a GitHub action.
"
Class {
	#name : #IceGitHubError,
	#superclass : #Error,
	#instVars : [
		'response',
		'jsonContents'
	],
	#category : #'Iceberg-Plugin-GitHub-Model'
}

{ #category : #private }
IceGitHubError class >> errorClassFor: aResponse [
	^ self allSubclasses 
		detect: [ :each | each isAppropriateFor: aResponse  ]
		ifNone: [ self ]
]

{ #category : #'instance creation' }
IceGitHubError class >> for: aResponse [
	^ (self errorClassFor: aResponse) new 
		response: aResponse;
		yourself	
]

{ #category : #testing }
IceGitHubError class >> isAppropriateFor: aResponse [
	^ false
]

{ #category : #'instance creation' }
IceGitHubError class >> signalFor: aResponse [
	^ (self for: aResponse) signal
]

{ #category : #accessing }
IceGitHubError >> code [
	^ self response status
]

{ #category : #accessing }
IceGitHubError >> contents [
	^ self response contents
]

{ #category : #private }
IceGitHubError >> customError: aDictionary [
	^ aDictionary at: #message
		
]

{ #category : #private }
IceGitHubError >> errorToString: aDictionary [ 
	(aDictionary at: #code) = 'custom' ifTrue: [ ^ self customError: aDictionary ].
	(aDictionary at: #code) = 'invalid' ifTrue: [ ^ self invalidError: aDictionary ].
	
	^ self unknownError: aDictionary	
]

{ #category : #private }
IceGitHubError >> invalidError: aDictionary [
	^ 'Invalid field: ', (aDictionary at: #field)
	
		
]

{ #category : #accessing }
IceGitHubError >> jsonContents [
	^ jsonContents ifNil: [ jsonContents := STON fromString: self contents ]	
	
]

{ #category : #accessing }
IceGitHubError >> messageBody [
	| errors |
	errors := self jsonContents at: #errors ifAbsent: [ ^ '' ].
	^ String streamContents: [ :stream | 
		errors
			do: [ :each | stream << (self errorToString: each) ] 
			separatedBy: [ stream cr ] ].
]

{ #category : #accessing }
IceGitHubError >> messageText [
	^ self jsonContents at: #message
]

{ #category : #accessing }
IceGitHubError >> response [
	^ response
]

{ #category : #accessing }
IceGitHubError >> response: anObject [
	response := anObject
]

{ #category : #private }
IceGitHubError >> unknownError: aDictionary [
	^ String streamContents: [ :stream | 
		(aDictionary associations sorted: [ :a :b | a key < b key  ])
			do: [ :each | stream << each key << ': ' << each value ]
			separatedBy: [ stream << ', ' ] ]
]
