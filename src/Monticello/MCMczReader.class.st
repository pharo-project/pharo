"
Reader for an MCZ format
"
Class {
	#name : 'MCMczReader',
	#superclass : 'MCAbstractMetadataFullReader',
	#instVars : [
		'zip',
		'filename'
	],
	#classVars : [
		'ReadingOldExport'
	],
	#category : 'Monticello-Storing',
	#package : 'Monticello',
	#tag : 'Storing'
}

{ #category : 'accessing' }
MCMczReader class >> extension [
	^ 'mcz'
]

{ #category : 'accessing' }
MCMczReader class >> on: s fileName: f [
	^ (self on: s)
		filename: f;
		yourself
]

{ #category : 'accessing' }
MCMczReader class >> readingOldExport [
	"Set true if you want to use the reader to import code that was exported before Pharo 12 and has the old MCClassDefinition format. This will be used by Pharo bootstrap for example until the bootstrap is done by a Pharo 12 image."

	^ ReadingOldExport ifNil: [ ReadingOldExport := false ]
]

{ #category : 'accessing' }
MCMczReader class >> readingOldExport: anObject [

	ReadingOldExport := anObject
]

{ #category : 'testing' }
MCMczReader class >> supportsDependencies [
	^ true
]

{ #category : 'testing' }
MCMczReader class >> supportsVersions [
	^ true
]

{ #category : 'parsing' }
MCMczReader >> contentsForMember: member [
	^[(member contentStreamFromEncoding: 'utf8') contents] on: ZnInvalidUTF8
		do: [:exc | 
			"Case of legacy encoding, presumably it is latin-1.
			But if contents starts with a null character, it might be a case of WideString encoded in UTF-32BE"
			| str |
			str := (member contentStreamFromEncoding: 'latin1').
			exc return: ((str peek = Character null and: [ str size \\ 4 = 0 ])
				ifTrue: [WideString fromByteArray: str contents asByteArray]
				ifFalse: [str contents])]
]

{ #category : 'parsing' }
MCMczReader >> extractDefinitionsFrom: member [

	| reader |
	(MCStReader canReadFileNamed: member fileName) ifFalse: [ ^ self ].
	reader := MCStReader on: (self contentsForMember: member) readStream.
	definitions addAll: reader definitions
]

{ #category : 'parsing' }
MCMczReader >> extractDependencyFrom: zipMember [
	^ MCVersionDependency
		package: (MCPackage named: (zipMember fileName copyAfterLast: $/))
		info: (self extractInfoFrom: (self parseMember: zipMember))
]

{ #category : 'accessing' }
MCMczReader >> filename [
	^ filename
]

{ #category : 'accessing' }
MCMczReader >> filename: aString [
	filename := aString
]

{ #category : 'loading' }
MCMczReader >> loadDefinitions [

	definitions := OrderedCollection new.
	(self zip membersMatching: 'snapshot/*') do: [ :m | self extractDefinitionsFrom: m ]
]

{ #category : 'loading' }
MCMczReader >> loadDependencies [
	dependencies := (self zip membersMatching: 'dependencies/*') collect: [:m | self extractDependencyFrom: m].
	dependencies := dependencies asArray.

]

{ #category : 'loading' }
MCMczReader >> loadVersionInfo [

	info := self extractInfoFrom: (self parseMember: 'version')
]

{ #category : 'parsing' }
MCMczReader >> parseMember: memberOrName [

	| member tokens |
	member := self zip member: memberOrName.
	tokens := (OCParser parseLiterals: (self contentsForMember: member)) first.
	^ self associate: tokens
]

{ #category : 'accessing' }
MCMczReader >> zip [
	stream closed ifTrue: [
		zip := nil].
	zip ifNil: [
		zip := ZipArchive new.
		zip readFrom: (stream closed
        ifTrue: [	filename asFileReference binaryReadStream ]
        ifFalse: [ stream ])
	].
	^ zip
]
