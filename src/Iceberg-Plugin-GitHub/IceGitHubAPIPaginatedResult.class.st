"
I'm a paginated answer from github (he sends results in pages and includes information in headers)
"
Class {
	#name : #IceGitHubAPIPaginatedResult,
	#superclass : #Object,
	#instVars : [
		'api',
		'request',
		'result',
		'nextPage'
	],
	#category : #'Iceberg-Plugin-GitHub-Model'
}

{ #category : #'as yet unclassified' }
IceGitHubAPIPaginatedResult class >> api: anAPI request: aString [
	^ self new 
		api: anAPI;
		request: aString;
		yourself
		
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> all [
	| stream |
	stream := Array new writeStream.
	[ self hasNext ] 
	whileTrue: [ stream << self next ].
	^ stream contents
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> api [
	^ api
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> api: anObject [
	api := anObject
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> hasNext [
	^ result isNil or: [ self parseMorePages: (result headers at: 'Link' ifAbsent: [ nil ]) ]
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> next [ 
	nextPage ifNil: [ nextPage := 1 ].
	[ result := self api getRaw: (self request asUrl ? (#page -> nextPage asString)) asString ]
	ensure: [ nextPage := nextPage + 1].
	^ STON fromString: result contents
]

{ #category : #private }
IceGitHubAPIPaginatedResult >> parseMorePages: aString [ 
	"github does not send 'Links' header if there are no pages, and it does not sends 'next' link
	 if we are at the last page"
	aString ifNil: [ ^ false ].
	^ (aString substrings: ',') anySatisfy: [ :each | each includesSubstring: 'rel="next"' ] 
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> request [
	^ request
]

{ #category : #accessing }
IceGitHubAPIPaginatedResult >> request: anObject [
	request := anObject
]
