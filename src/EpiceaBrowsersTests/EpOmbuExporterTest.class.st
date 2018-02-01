Class {
	#name : #EpOmbuExporterTest,
	#superclass : #TestCase,
	#instVars : [
		'outputLog',
		'inputLog1',
		'inputLog2',
		'logBuilder1',
		'logBuilder2'
	],
	#category : #'EpiceaBrowsersTests-Exporter'
}

{ #category : #accessing }
EpOmbuExporterTest >> outputLogNameForTest [

	^ self className, 'LogForTest'
]

{ #category : #running }
EpOmbuExporterTest >> setUp [ 
	super setUp.
	
	logBuilder1 := EpTestLogBuilder new.
	logBuilder2 := EpTestLogBuilder new.	
	inputLog1 := logBuilder1 oneTriggerWithFourChildrenLog.
	inputLog2 := logBuilder2 oneTriggerWithOneChildLog.
	outputLog := EpLog newWithStore: OmFileStore new.

]

{ #category : #running }
EpOmbuExporterTest >> tearDown [
	
	logBuilder1 cleanUp.
	logBuilder2 cleanUp.

	(FileSystem workingDirectory / (self outputLogNameForTest, '.ombu')) ensureDelete.

	super tearDown.
]

{ #category : #tests }
EpOmbuExporterTest >> testBasicExport [
	| selection outputResult |
	
	selection := inputLog1 entries.
	
	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: selection.
	
	outputResult := outputLog events.
	
	self assert: outputResult size equals: selection size.
	self 
		assert: outputResult asSet 
		equals: (selection collect: [ :each | each content ]) asSet
		
]

{ #category : #tests }
EpOmbuExporterTest >> testCommentExportWithDependencyFix [

	| selection oldComment newComment |
	oldComment := nil.
	selection := inputLog2 entries.
	
	inputLog2 store entriesDo: [ :entry |
		entry content isEpLogEntriesComment ifTrue: [
			oldComment := entry ]
		].
	
	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: selection.
		
	self assert: outputLog entries size equals: selection size.
	
	outputLog store entriesDo: [ :entry |
		entry content isEpLogEntriesComment ifTrue: [
			newComment := entry ]
		].

	self deny: ((oldComment tagAt: EpLog priorReferenceKey) = (newComment tagAt: EpLog priorReferenceKey)).
	self deny: ((oldComment tagAt: OmStore selfReferenceKey) = (newComment tagAt: OmStore selfReferenceKey)).
	self deny: (oldComment content entryReferences = newComment content entryReferences)
	
]

{ #category : #tests }
EpOmbuExporterTest >> testRoundtrip [

	| aLog anotherLog |
	aLog := 
		EpLog newWithStore: 
			(OmFileStore 
				named: self outputLogNameForTest
				inDirectory: FileSystem workingDirectory).

	EpOmbuExporter new 
		outputLog: aLog;
		fileOut: inputLog1 entries.

	anotherLog := 
		EpLog newWithStore: 
			(OmFileStore 
				named: self outputLogNameForTest
				inDirectory: FileSystem workingDirectory).

	self assert: anotherLog entries size equals: inputLog1 entries size.
]
