"
Reader for an MCZ format
"
Class {
	#name : 'MCMczReader',
	#superclass : 'MCVersionReader',
	#instVars : [
		'zip',
		'infoCache',
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

{ #category : 'loading' }
MCMczReader >> adaptDefinitions [
	"In the old export format the 4th variable of a MCClassDefinition was a category. Now we have the package name and in the last variables we have the tag.
	This is allowing us to read the old format and try to find the package and the tag. This is not bulletproof."

	self class readingOldExport ifFalse: [ ^ self ].
	definitions
		select: [ :def | def isClassDefinition ]
		thenDo: [ :definition |
			| converter |
			converter := CategoryConverter category: definition packageName.
			definition
				packageName: converter packageName;
				tagName: converter tagName ]
]

{ #category : 'converting' }
MCMczReader >> associate: tokens [
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

{ #category : 'utilities' }
MCMczReader >> extractInfoFrom: dict [
	^MCWorkingCopy infoFromDictionary: dict cache: self infoCache
]

{ #category : 'accessing' }
MCMczReader >> filename [
	^ filename
]

{ #category : 'accessing' }
MCMczReader >> filename: aString [
	filename := aString
]

{ #category : 'accessing' }
MCMczReader >> infoCache [
	^ infoCache ifNil: [infoCache := Dictionary new]
]

{ #category : 'loading' }
MCMczReader >> loadDefinitions [

	definitions := OrderedCollection new.
	(self zip memberNamed: 'snapshot.bin') ifNotNil: [ :m |
		[
		definitions := (MCDataStream on: m contents readStream) next definitions.
		self adaptDefinitions.
		^ definitions ]
			on: Error
			do: [ :fallThrough |
				('An error happened while reading MCZ. We will fallback to another format. Short error stack: ' , fallThrough signalerContext shortStack) traceCr ] ].
	"otherwise"
	(self zip membersMatching: 'snapshot/*') do: [ :m | self extractDefinitionsFrom: m ]
]

{ #category : 'loading' }
MCMczReader >> loadDependencies [
	dependencies := (self zip membersMatching: 'dependencies/*') collect: [:m | self extractDependencyFrom: m].
	dependencies := dependencies asArray.

]

{ #category : 'loading' }
MCMczReader >> loadPackage [
	| dict |
	dict := self parseMember: 'package'.
	package := MCPackage named: (dict at: #name)
]

{ #category : 'loading' }
MCMczReader >> loadVersionInfo [

	info := self extractInfoFrom: (self parseMember: 'version')
]

{ #category : 'parsing' }
MCMczReader >> parseMember: memberOrName [
	| member tokens |
	member := self zip member: memberOrName.
	tokens := (self contentsForMember: member) parseLiterals first.
	^ self associate: tokens
]

{ #category : 'accessing' }
MCMczReader >> scanner [
	^ MCScanner
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
