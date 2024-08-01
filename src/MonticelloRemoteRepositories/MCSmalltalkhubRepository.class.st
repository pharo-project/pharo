"
I am specialized version of an MCHttpRepository for http://smalltalkhub.com.
I support a faster mcz listing that does not rely on parsing an html size.
"
Class {
	#name : 'MCSmalltalkhubRepository',
	#superclass : 'MCHttpRepository',
	#instVars : [
		'owner',
		'project'
	],
	#category : 'MonticelloRemoteRepositories',
	#package : 'MonticelloRemoteRepositories'
}

{ #category : 'accessing' }
MCSmalltalkhubRepository class >> baseURL [
	^ self smalltalkhubUrl, 'mc/'
]

{ #category : 'accessing' }
MCSmalltalkhubRepository class >> description [
	^ 'smalltalkhub.com'
]

{ #category : 'accessing' }
MCSmalltalkhubRepository class >> hostName [
	^ 'smalltalkhub.com'
]

{ #category : 'testing' }
MCSmalltalkhubRepository class >> isResponsibleFor: aUrl [
	^ aUrl includesSubstring: self hostName
]

{ #category : 'instance creation' }
MCSmalltalkhubRepository class >> owner: owner project: project [ 
	^ self
		owner: owner
		project: project
		user: String empty
		password: String empty
]

{ #category : 'instance creation' }
MCSmalltalkhubRepository class >> owner: owner project: project user: user password: password [
	^ self new
		owner: owner;
		project: project;
		user: user;
		password: password;
		yourself
]

{ #category : 'accessing' }
MCSmalltalkhubRepository class >> smalltalkhubUrl [
	^ 'http://', self hostName, '/'
]

{ #category : 'interface' }
MCSmalltalkhubRepository >> includesFileNamed: aString [
	"avoid the slower default method and simply do a head request "
	self httpClient
		numberOfRetries: 0;
		ifFail: [ :exception | 
			((exception isKindOf: ZnHttpUnsuccessful) and: [ exception response isNotFound ])
				ifTrue: [ ^ false ].
			exception pass];
		head: (self urlForFileNamed: aString).
	^ true
]

{ #category : 'interface' }
MCSmalltalkhubRepository >> includesVersionNamed: aString [
	"directly do a filename check since the server only stores mcz"
	^ self includesFileNamed: aString, '.mcz'
]

{ #category : 'interface' }
MCSmalltalkhubRepository >> loadAllFileNames [
	| client |
	client := self httpClient.
	client
		ifFail: [ :exception | self error: 'Could not access ', self location, ': ', exception printString ];
		url: self locationWithTrailingSlash;
		queryAt: 'format' put: 'raw';
		get.
	self assertNonBinaryResponse: client response.
	^ self parseFileNamesFromStream: client contents
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> location [
	^ 'http://smalltalkhub.com/mc/', self owner, '/', self project, '/main/' 
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> location: aUrlString [
	| pathSegments |
	(self class isResponsibleFor: aUrlString)
		ifFalse: [ Error signal: 'Please provide a smalltalkhub url' ].
	"extract the smalltalkhub properties from the path part in the given URL"
	pathSegments := aUrlString asZnUrl pathSegments reject: [ :each | each = 'mc' ].
	self owner: pathSegments first.
	self project: pathSegments second.
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> locationWithTrailingSlash [
	^ self location
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> owner [
	
	^ owner
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> owner: aString [
	
	owner := aString
]

{ #category : 'interface' }
MCSmalltalkhubRepository >> parseFileNamesFromStream: aNewLineDelimitedString [
	^ aNewLineDelimitedString 
		ifNil: [ ^ OrderedCollection new ]
		ifNotNil: [ aNewLineDelimitedString substrings: String crlf ]
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> project [
	
	^ project
]

{ #category : 'accessing' }
MCSmalltalkhubRepository >> project: aString [
	
	project := aString
]
