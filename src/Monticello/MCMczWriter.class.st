"
Writing MCZ format
"
Class {
	#name : 'MCMczWriter',
	#superclass : 'MCWriter',
	#instVars : [
		'zip',
		'writtenInfo'
	],
	#category : 'Monticello-Storing',
	#package : 'Monticello',
	#tag : 'Storing'
}

{ #category : 'writing' }
MCMczWriter class >> fileOut: aVersion on: aStream [
	| inst |
	inst := self on: aStream.
	inst writeVersion: aVersion.
	inst flush.
	

]

{ #category : 'accessing' }
MCMczWriter class >> readerClass [
	^ MCMczReader
]

{ #category : 'writing' }
MCMczWriter >> addString: string at: path encodedTo: encodingName [
	| member |
	member := zip addString: (string encodeWith: encodingName) as: path.
	member desiredCompressionMethod: ZipArchive compressionDeflated 
	
]

{ #category : 'writing' }
MCMczWriter >> flush [
	zip writeTo: stream.
]

{ #category : 'initialization' }
MCMczWriter >> initialize [
	super initialize.
	zip := ZipArchive new.

]

{ #category : 'private' }
MCMczWriter >> isInfoWritten: aVersionInfo [
	^ self writtenInfo includes: aVersionInfo
]

{ #category : 'serializing' }
MCMczWriter >> serializeDefinitions: aCollection [
	^String streamContents: [:aStream |
		| writer |
		writer := self snapshotWriterClass on: aStream.
		writer writeDefinitions: aCollection]
]

{ #category : 'serializing' }
MCMczWriter >> serializePackage: aPackage [
	^ '(name ''', aPackage name, ''')'
]

{ #category : 'serializing' }
MCMczWriter >> serializeVersionInfo: aVersionInfo [

	^ String streamContents: [ :aStream | self writeVersionInfo: aVersionInfo on: aStream ]
]

{ #category : 'accessing' }
MCMczWriter >> snapshotWriterClass [
	^ MCStWriter
]

{ #category : 'visiting' }
MCMczWriter >> writeDefinitions: aVersion [
	self writeSnapshot: aVersion snapshot
]

{ #category : 'visiting' }
MCMczWriter >> writePackage: aPackage [
	self addString: (self serializePackage: aPackage) at: 'package' encodedTo: 'utf8'
]

{ #category : 'visiting' }
MCMczWriter >> writeSnapshot: aSnapshot [

	self
		addString: (self serializeDefinitions: aSnapshot definitions)
		at: 'snapshot/source.' , self snapshotWriterClass extension
		encodedTo: 'utf8'
]

{ #category : 'visiting' }
MCMczWriter >> writeVersion: aVersion [
	self writePackage: aVersion package.
	self writeVersionInfo: aVersion info.
	self writeDefinitions: aVersion.
	aVersion dependencies do: [:ea | self writeVersionDependency: ea]
]

{ #category : 'visiting' }
MCMczWriter >> writeVersionDependency: aVersionDependency [
	| string |
	string := (self serializeVersionInfo: aVersionDependency versionInfo).
	self addString: string at: 'dependencies/', aVersionDependency package name encodedTo: 'utf8'
]

{ #category : 'visiting' }
MCMczWriter >> writeVersionInfo: aVersionInfo [
	| string |
	string := self serializeVersionInfo: aVersionInfo.
	self addString: string at: 'version' encodedTo: 'utf8'.

]

{ #category : 'visiting' }
MCMczWriter >> writeVersionInfo: aVersionInfo on: aStream [

	(self isInfoWritten: aVersionInfo) ifTrue: [
			^ aStream
				  nextPutAll: '(id ';
				  print: aVersionInfo id asString;
				  nextPut: $) ].
	aStream nextPut: $(.
	#( name message id date time author ) do: [ :sel |
			| toWrite |
			toWrite := (aVersionInfo perform: sel) ifNil: [ '' ].
			sel = #date ifTrue: [ toWrite := toWrite yyyymmdd ].
			aStream
				nextPutAll: sel;
				space;
				print: (toWrite asString encodeWith: #utf8) asString;
				space ].
	aStream nextPutAll: 'ancestors ('.
	aVersionInfo ancestors do: [ :ea | self writeVersionInfo: ea on: aStream ].
	aStream nextPutAll: ') stepChildren ('.
	aVersionInfo stepChildren do: [ :ea | self writeVersionInfo: ea on: aStream ].
	aStream nextPutAll: '))'.

	self wroteInfo: aVersionInfo
]

{ #category : 'private' }
MCMczWriter >> writtenInfo [

	^ writtenInfo ifNil: [ writtenInfo := Set new ]
]

{ #category : 'private' }
MCMczWriter >> wroteInfo: aVersionInfo [
	self writtenInfo add: aVersionInfo
]

{ #category : 'accessing' }
MCMczWriter >> zip [
	^ zip
]
