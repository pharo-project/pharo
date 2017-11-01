"
I know how to export Epicea entries from one log to a new one.
"
Class {
	#name : #EpOmbuExporter,
	#superclass : #Object,
	#instVars : [
		'outputLog'
	],
	#category : 'EpiceaBrowsers-Exporter'
}

{ #category : #convenience }
EpOmbuExporter class >> askFileNameAndFileOut: entries in: aDirectory [

	| fileNameOrNil outputLog |
	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension)'
		initialAnswer: 'FileOut_', (UUID new asString36 first: 3).

	fileNameOrNil ifNil: [ ^ self ].

	(OmFileStore existsStoreNamed: fileNameOrNil inDirectory: aDirectory)
		ifTrue: [ ^ self inform: 'File already exists' ].

	outputLog := EpLog newWithStore: 
		(OmStoreFactory current named: fileNameOrNil inDirectory: aDirectory).

	self new 
		outputLog: outputLog;
		fileOut: entries.
]

{ #category : #tests }
EpOmbuExporter >> fileOut: epiceaEntries [

	| originalReference anEvent exportedReference visitor map |	
	map := Dictionary new.
	visitor := EpReferenceFixerVisitor newWithMap: map.
	
	epiceaEntries do: [ :anEntry | 
		originalReference := (anEntry tagAt: OmStore selfReferenceKey).

		anEvent := anEntry content accept: visitor.
		
		exportedReference := outputLog addEntryWith: anEvent tags: [ :tags |  ].
		
		map at: originalReference put: exportedReference.
		].
	
	outputLog store flush.
]

{ #category : #accessing }
EpOmbuExporter >> outputLog [
	^ outputLog
]

{ #category : #accessing }
EpOmbuExporter >> outputLog: anObject [
	outputLog := anObject
]
