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
	#category : #'EpiceaBrowsers-Tests-Integration'
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

{ #category : #running }
EpLogBrowserOperationFactoryTest >> tearDown [

	self flag: #categories. "This tear down should not be needed but it currently is."
	self packageOrganizer unregisterPackageNamed: classFactory packageName.
	self packageOrganizer unregisterPackageNamed: classFactory defaultCategory.
	super tearDown
]
