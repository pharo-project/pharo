"
Writing MCZ format
"
Class {
	#name : #MCMczWriter,
	#superclass : #MCWriter,
	#instVars : [
		'zip',
		'infoWriter'
	],
	#category : #'Monticello-Storing'
}

{ #category : #writing }
MCMczWriter class >> fileOut: aVersion on: aStream [
	| inst |
	inst := self on: aStream.
	inst writeVersion: aVersion.
	inst flush.
	

]

{ #category : #accessing }
MCMczWriter class >> readerClass [
	^ MCMczReader
]

{ #category : #writing }
MCMczWriter >> addString: string at: path [
	| member |
	member := zip addString: string as: path.
	member desiredCompressionMethod: ZipArchive compressionDeflated 
	
]

{ #category : #writing }
MCMczWriter >> addString: string at: path encodedTo: encodingName [
	| member |
	member := zip addString: (string convertToEncoding: encodingName) as: path.
	member desiredCompressionMethod: ZipArchive compressionDeflated 
	
]

{ #category : #writing }
MCMczWriter >> flush [
	zip writeTo: stream.
]

{ #category : #accessing }
MCMczWriter >> format [
	^ '1'
]

{ #category : #initialization }
MCMczWriter >> initialize [
	super initialize.
	zip := ZipArchive new.

]

{ #category : #serializing }
MCMczWriter >> serializeDefinitions: aCollection [
	^String streamContents: [:aStream |
		| writer |
		writer := self snapshotWriterClass on: aStream.
		writer writeDefinitions: aCollection]
]

{ #category : #serializing }
MCMczWriter >> serializeInBinary: aSnapshot [
	| writer s |
	s := RWBinaryOrTextStream on: String new.
	writer := MCDataStream on: s.
	writer nextPut: aSnapshot.
	^ s contents
]

{ #category : #serializing }
MCMczWriter >> serializePackage: aPackage [
	^ '(name ''', aPackage name, ''')'
]

{ #category : #serializing }
MCMczWriter >> serializeVersionInfo: aVersionInfo [
	infoWriter ifNil: [infoWriter := MCVersionInfoWriter new].
	^ String streamContents: [:s |
		infoWriter stream: s.
		infoWriter writeVersionInfo: aVersionInfo]
]

{ #category : #accessing }
MCMczWriter >> snapshotWriterClass [
	^ MCStWriter
]

{ #category : #visiting }
MCMczWriter >> writeDefinitions: aVersion [
	self writeSnapshot: aVersion snapshot
]

{ #category : #visiting }
MCMczWriter >> writeFormat [
"	self addString: self format at: 'format'."
]

{ #category : #visiting }
MCMczWriter >> writePackage: aPackage [
	self addString: (self serializePackage: aPackage) at: 'package' encodedTo: 'utf8'
]

{ #category : #visiting }
MCMczWriter >> writeSnapshot: aSnapshot [
	self addString: (self serializeDefinitions: aSnapshot definitions) at: 'snapshot/source.', self snapshotWriterClass extension encodedTo: 'utf8'.
	self addString: (self serializeInBinary: aSnapshot) at: 'snapshot.bin'
]

{ #category : #visiting }
MCMczWriter >> writeVersion: aVersion [
	self writeFormat.
	self writePackage: aVersion package.
	self writeVersionInfo: aVersion info.
	self writeDefinitions: aVersion.
	aVersion dependencies do: [:ea | self writeVersionDependency: ea]
]

{ #category : #visiting }
MCMczWriter >> writeVersionDependency: aVersionDependency [
	| string |
	string := (self serializeVersionInfo: aVersionDependency versionInfo).
	self addString: string at: 'dependencies/', aVersionDependency package name encodedTo: 'utf8'
]

{ #category : #visiting }
MCMczWriter >> writeVersionInfo: aVersionInfo [
	| string |
	string := self serializeVersionInfo: aVersionInfo.
	self addString: string at: 'version' encodedTo: 'utf8'.

]

{ #category : #accessing }
MCMczWriter >> zip [
	^ zip
]
