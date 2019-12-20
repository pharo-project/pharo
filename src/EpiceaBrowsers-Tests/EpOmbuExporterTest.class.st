Class {
	#name : #EpOmbuExporterTest,
	#superclass : #TestCase,
	#instVars : [
		'outputLog',
		'inputLog1',
		'inputLog2',
		'outputDirectory',
		'logBuilder1',
		'logBuilder2'
	],
	#category : #'EpiceaBrowsers-Tests-Exporter'
}

{ #category : #running }
EpOmbuExporterTest >> setUp [

	super setUp.

	logBuilder1 := EpTestLogBuilder new.
	logBuilder2 := EpTestLogBuilder new.

	"Create 2 logs in different directories"
	inputLog1 := logBuilder1 oneTriggerWithFourChildrenLog.
	inputLog2 := logBuilder2 oneTriggerWithOneChildLog.

	outputDirectory := logBuilder2 directory.
	outputLog := EpLog
		newWithStore: (OmFileStore named: 'exported' inDirectory: outputDirectory)
]

{ #category : #running }
EpOmbuExporterTest >> tearDown [
	
	logBuilder1 cleanUp.
	logBuilder2 cleanUp.
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

	inputLog2 store entriesDo: [ :entry | entry content isEpLogEntriesComment ifTrue: [ oldComment := entry ] ].

	EpOmbuExporter new
		outputLog: outputLog;
		fileOut: selection.

	self assert: outputLog entries size equals: selection size.

	outputLog store entriesDo: [ :entry | entry content isEpLogEntriesComment ifTrue: [ newComment := entry ] ].

	self deny: (oldComment tagAt: EpLog priorReferenceKey) equals: (newComment tagAt: EpLog priorReferenceKey).
	self deny: (oldComment tagAt: OmStore selfReferenceKey) equals: (newComment tagAt: OmStore selfReferenceKey).
	self deny: oldComment content entryReferences equals: newComment content entryReferences
]

{ #category : #tests }
EpOmbuExporterTest >> testRoundtrip [

	| outputLogReopened |
	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: inputLog1 entries.

	"Open a fresh log with the exported entries. 
	(fresh, to be sure it was correctly written to disk)"
	outputLogReopened := EpLog newWithStore: outputLog store flush copyReopened refresh.

	self
		assert: outputLogReopened entries size
		equals: inputLog1 entries size
]
