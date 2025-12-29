"
I am an monticello repository implementation for the FTP protocol.
"
Class {
	#name : 'MCFtpRepository',
	#superclass : 'MCRemoteFileBasedRepository',
	#instVars : [
		'host',
		'directory',
		'user',
		'password'
	],
	#category : 'MonticelloRemoteRepositories',
	#package : 'MonticelloRemoteRepositories'
}

{ #category : 'instance creation' }
MCFtpRepository class >> basicFromUrl: aZnUrl [
	^ self
		host: (aZnUrl hasPort ifTrue: [ aZnUrl host, ':', aZnUrl port asString ] ifFalse: [ aZnUrl host ])
		directory: aZnUrl path "MCFtpRepository assumes NO prefixed / in the path"
		user: (aZnUrl username ifNil: [ '' ])
		password: (aZnUrl password ifNil: [ '' ])
]

{ #category : 'instance creation' }
MCFtpRepository class >> createRepositoryFromSpec: aRepositorySpec [

	| description headerSize index |
	description := aRepositorySpec description.
	headerSize := 'ftp://' size.
	index := description indexOf: $/ startingAt: headerSize + 1.

	^ self
		  host: (description copyFrom: headerSize + 1 to: index - 1)
		  directory: (description copyFrom: index + 1 to: description size)
		  user: aRepositorySpec username
		  password: aRepositorySpec password
]

{ #category : 'accessing' }
MCFtpRepository class >> description [
	^ 'FTP'
]

{ #category : 'instance creation' }
MCFtpRepository class >> host: host directory: directory user: user password: password [
	^ self new
		host: host;
		directory: directory;
		user: user;
		password: password
]

{ #category : 'testing' }
MCFtpRepository class >> isAvailableFor: type [
	^ type = 'ftp'
]

{ #category : 'accessing' }
MCFtpRepository class >> urlSchemes [
	^ #(ftp)
]

{ #category : 'converting' }
MCFtpRepository >> asRepositorySpecFor: aMetacelloProject [
	| dir |
	dir := directory.
	(directory at: 1) = $/
		ifFalse: [ dir := '/', dir ].
	^(aMetacelloProject repositorySpec)
		description:  'ftp://', host, dir;
	 	type: 'ftp';
		username: user;
		password: password;
		yourself
]

{ #category : 'enumerating' }
MCFtpRepository >> clientDo: aBlock [
	| client |
	client := FTPClient openOnHostNamed: host.
	client loginUser: user password: password.
	directory isEmpty ifFalse: [client changeDirectoryTo: directory].
	^ [aBlock value: client] ensure: [client close]
]

{ #category : 'required' }
MCFtpRepository >> description [
	^ 'ftp://', host, '/', directory
]

{ #category : 'accessing' }
MCFtpRepository >> directory [

	^directory
]

{ #category : 'accessing' }
MCFtpRepository >> directory: dirPath [
	directory := dirPath
]

{ #category : 'accessing' }
MCFtpRepository >> host [

	^host
]

{ #category : 'accessing' }
MCFtpRepository >> host: hostname [
	host := hostname
]

{ #category : 'interface' }
MCFtpRepository >> loadAllFileNames [
	^ self clientDo:
		[:client |
		self parseDirectoryListing: client getDirectory]
]

{ #category : 'parsing' }
MCFtpRepository >> parseDirectoryListing: aString [
	| stream files line tokens |
	stream := aString readStream.
	files := OrderedCollection new.
	[stream atEnd] whileFalse:
		[line := stream nextLine.
		tokens := line findTokens: ' '.
		tokens size > 2 ifTrue: [files add: tokens last]].
	^ files
]

{ #category : 'accessing' }
MCFtpRepository >> password: passwordString [
	password := passwordString
]

{ #category : 'required' }
MCFtpRepository >> readStreamForFileNamed: aString do: aBlock [
	
	^ self clientDo: [:client | 
		client binary.
		aBlock value: (client getFileNamed: aString) asByteArray readStream]
]

{ #category : 'accessing' }
MCFtpRepository >> user: userString [
	user := userString
]

{ #category : 'required' }
MCFtpRepository >> writeStreamForFileNamed: aString do: aBlock [

	self clientDo: [ :client |
		client binary.
		client putFileStreamContents: (ByteArray streamContents: aBlock) readStream as: aString ]
]
