"
An installer for Monticello files with the *.mcz extension. 

   MczInstaller installFileNamed: 'MyKillerApp-Core-Author.1.mcz'
"
Class {
	#name : #MczInstaller,
	#superclass : #Object,
	#instVars : [
		'stream',
		'zip'
	],
	#classVars : [
		'Versions'
	],
	#category : #'System-Installers'
}

{ #category : #versioninfo }
MczInstaller class >> clearVersionInfo [
	Versions := Dictionary new
]

{ #category : #'System-FileRegistry' }
MczInstaller class >> extension [
	^ 'mcz'
]

{ #category : #'System-FileRegistry' }
MczInstaller class >> fileReaderServicesForFile: fileName suffix: suffix [
	<fileService>
	^({ self extension. '*' } includes: suffix)
		ifTrue: [ self services ]
		ifFalse: [#()].

]

{ #category : #'System-FileRegistry' }
MczInstaller class >> initialize [
	self clearVersionInfo
]

{ #category : #installing }
MczInstaller class >> installFileNamed: aFileName [
	self installStream: aFileName asFileReference binaryReadStream
]

{ #category : #installing }
MczInstaller class >> installStream: aStream [
	(self on: aStream) install
]

{ #category : #'System-FileRegistry' }
MczInstaller class >> loadVersionFile: fileName [
	self installFileNamed: fileName

]

{ #category : #'instance creation' }
MczInstaller class >> on: aStream [
	^ self new stream: aStream
]

{ #category : #'System-FileRegistry' }
MczInstaller class >> serviceLoadVersion [
	^ SimpleServiceEntry
		provider: self
		label: 'Load'
		selector: #loadVersionFile:
		description: 'Load a package version'
]

{ #category : #'System-FileRegistry' }
MczInstaller class >> services [
	^ Array with: self serviceLoadVersion
]

{ #category : #versioninfo }
MczInstaller class >> storeVersionInfo: aVersion [
	Versions 
		at: aVersion package name
		put: aVersion info asDictionary
]

{ #category : #versioninfo }
MczInstaller class >> versionInfo [
	^ Versions
]

{ #category : #utilities }
MczInstaller >> associate: tokens [
	| result |
	result := Dictionary new.
	tokens pairsDo: [:key :value | 
					| tmp |
					tmp := value.
					value isString ifFalse: [tmp := value collect: [:ea | self associate: ea]].
					value = 'nil' ifTrue: [tmp := ''].
					result at: key put: tmp].
	^ result
]

{ #category : #utilities }
MczInstaller >> checkDependencies [
	| dependencies unmet |
	dependencies := (zip membersMatching: 'dependencies/*') 
			collect: [:member | self extractInfoFrom: (self parseMember: member)].
	unmet := dependencies reject: [:dep |
		self versions: Versions anySatisfy: (dep at: #id)].
	^ unmet isEmpty or: [
		self confirm: (String streamContents: [:s|
			s nextPutAll: 'The following dependencies seem to be missing:'; cr.
			unmet do: [:each | s nextPutAll: (each at: #name); cr].
			s nextPutAll: 'Do you still want to install this package?'])]
]

{ #category : #private }
MczInstaller >> contentStreamForMember: member [
	^[(member contentStreamFromEncoding: 'utf8') text] on: ZnInvalidUTF8
		do: [:exc | 
			"Case of legacy encoding, presumably it is latin-1 and we do not need to do anything
			But if contents starts with a null character, it might be a case of WideString encoded in UTF-32BE"
			| str |
			str := (member contentStreamFromEncoding: 'latin1') text.
			(str peek = Character null and: [ str size \\ 4 = 0 ])
				ifTrue: [str := (WideString fromByteArray: str contents asByteArray) readStream].
			exc return: str]
]

{ #category : #parsing }
MczInstaller >> contentsForMember: member [
	^[(member contentStreamFromEncoding: 'utf8') text contents] on: ZnInvalidUTF8
		do: [:exc | 
			"Case of legacy encoding, presumably it is latin-1.
			But if contents starts with a null character, it might be a case of WideString encoded in UTF-32BE"
			| str |
			str := (member contentStreamFromEncoding: 'latin1') text.
			exc return: ((str peek = Character null and: [ str size \\ 4 = 0 ])
				ifTrue: [WideString fromByteArray: str contents asByteArray]
				ifFalse: [str contents])]
]

{ #category : #utilities }
MczInstaller >> extractInfoFrom: dict [
	dict at: #id put: (UUID fromString: (dict at: #id)).
	dict at: #date ifPresent: [:d | d isEmpty ifFalse: [dict at: #date put: (Date fromString: d)]].
	dict at: #time ifPresent: [:t | t isEmpty ifFalse: [dict at: #time put: (Time readFrom: t readStream)]].
	dict at: #ancestors ifPresent: [:a | dict at: #ancestors put: (a collect: [:ea | self extractInfoFrom: ea])].
	^ dict
]

{ #category : #accessing }
MczInstaller >> extractPackageName [
	^ (self parseMember: 'package') at: #name.
	
]

{ #category : #accessing }
MczInstaller >> extractVersionInfo [
	^ self extractInfoFrom: (self parseMember: 'version')
]

{ #category : #installation }
MczInstaller >> install [
	| sources |
	zip := ZipArchive new.
	zip readFrom: stream.
	self checkDependencies ifFalse: [^false].
	self recordVersionInfo.
	sources := (zip membersMatching: 'snapshot/*') 
				asSortedCollection: [:a :b | a fileName < b fileName].
	sources do: [:src | self installMember: src].
]

{ #category : #installation }
MczInstaller >> installMember: member [
	 
	self useNewChangeSetDuring:
		[ CodeImporter evaluateReadStream: (self contentsForMember: member) readStream.
		]
]

{ #category : #utilities }
MczInstaller >> parseMember: memberOrName [
	| member tokens |
	member := self zip member: memberOrName.
	tokens := (self contentsForMember: member) parseLiterals first.
	^ self associate: tokens
]

{ #category : #accessing }
MczInstaller >> recordVersionInfo [
	Versions 
		at: self extractPackageName 
		put: self extractVersionInfo
]

{ #category : #accessing }
MczInstaller >> stream: aStream [
	stream := aStream
]

{ #category : #utilities }
MczInstaller >> useNewChangeSetDuring: aBlock [
	| changeHolder oldChanges newChanges |
	changeHolder := (ChangeSet respondsTo: #newChanges:)
						ifTrue: [ChangeSet]
						ifFalse: [Smalltalk].
	oldChanges := (ChangeSet respondsTo: #current)
						ifTrue: [ChangeSet current]
						ifFalse: [Smalltalk changes].

	newChanges := ChangeSet new name: (ChangeSet uniqueNameLike: self extractPackageName).
	changeHolder newChanges: newChanges.
	[aBlock value] ensure: [changeHolder newChanges: oldChanges].
]

{ #category : #utilities }
MczInstaller >> versions: aVersionList anySatisfy: aDependencyID [
	^ aVersionList anySatisfy: [:version | 
			aDependencyID = (version at: #id)
				or: [self versions: (version at: #ancestors) anySatisfy: aDependencyID]]
]

{ #category : #accessing }
MczInstaller >> zip [
	^zip
]
