"
I test EpMonitor integration (either enabled or disabled).
"
Class {
	#name : 'EpMonitorIntegrationTest',
	#superclass : 'TestCase',
	#instVars : [
		'monitor',
		'classFactory',
		'logBuilder'
	],
	#category : 'Epicea-Tests-Integration',
	#package : 'Epicea-Tests',
	#tag : 'Integration'
}

{ #category : 'asserting' }
EpMonitorIntegrationTest class >> shouldLogWithEpicea [

	^ true
]

{ #category : 'running' }
EpMonitorIntegrationTest >> allLogEntriesWith: aClass [

	^ monitor log entries select: [ :each | each content class = aClass ]
]

{ #category : 'running' }
EpMonitorIntegrationTest >> allLogEventsWith: aClass [

	^ (self allLogEntriesWith: aClass) collect: [:each | each content]
]

{ #category : 'running' }
EpMonitorIntegrationTest >> countLogEventsWith: aClass [

	^ (self allLogEntriesWith: aClass) size
]

{ #category : 'running' }
EpMonitorIntegrationTest >> setUp [
	super setUp.

	logBuilder := EpTestLogBuilder new
		useLogWithSessionStore;
		yourself.
	
	"Epicea works, sadly, with the global environment only"
	classFactory := ClassFactoryForTestCase environment: self class environment.
	monitor := EpMonitor newWithLog: logBuilder log
]

{ #category : 'running' }
EpMonitorIntegrationTest >> tearDown [

	monitor disable.
	classFactory cleanUp.
	logBuilder cleanUp.

	super tearDown
]
