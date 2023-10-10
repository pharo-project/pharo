"
My subclasses test EpLogBrowserModel operations.
"
Class {
	#name : 'EpLogBrowserOperationFactoryTest',
	#superclass : 'EpEnabledIntegrationTest',
	#instVars : [
		'inputEntry',
		'inputEntries'
	],
	#category : 'EpiceaBrowsers-Tests-Integration',
	#package : 'EpiceaBrowsers-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
EpLogBrowserOperationFactoryTest >> setHeadAsInputEntry [

	inputEntry := monitor log head.
	inputEntries := { monitor log head }
]

{ #category : 'tests' }
EpLogBrowserOperationFactoryTest >> setMonitorLogAsInputEntries [

	inputEntries := monitor log entries.
	inputEntry := nil. "Not used"
]

{ #category : 'running' }
EpLogBrowserOperationFactoryTest >> tearDown [

	(self packageOrganizer packages select: [ :package | package name beginsWith: classFactory packageName ]) do: [ :package | package removeFromSystem ].
	super tearDown
]
