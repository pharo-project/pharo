"
My subclasses test EpLogBrowserModel operations.
"
Class {
	#name : #EpLogBrowserOperationFactoryTest,
	#superclass : #EpEnabledIntegrationTest,
	#instVars : [
		'inputEntry',
		'inputEntries'
	],
	#category : #EpiceaBrowsersTests-Integration
}

{ #category : #tests }
EpLogBrowserOperationFactoryTest >> setHeadAsInputEntry [

	inputEntry := monitor log head.
	inputEntries := { monitor log head }
]

{ #category : #tests }
EpLogBrowserOperationFactoryTest >> setMonitorLogAsInputEntries [

	inputEntries := monitor log entries.
	inputEntry := nil. "Not used"
]
