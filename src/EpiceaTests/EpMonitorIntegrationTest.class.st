"
I test EpMonitor integration (either enabled or disabled).
"
Class {
	#name : #EpMonitorIntegrationTest,
	#superclass : #TestCase,
	#instVars : [
		'monitor',
		'classFactory',
		'logBuilder'
	],
	#category : #'EpiceaTests-Integration'
}

{ #category : #running }
EpMonitorIntegrationTest >> allLogEntriesWith: aClass [
	
	^ monitor log entries select: [ :each | each content class = aClass ]
]

{ #category : #running }
EpMonitorIntegrationTest >> allLogEventsWith: aClass [

	^ (self allLogEntriesWith: aClass) collect: #content
]

{ #category : #running }
EpMonitorIntegrationTest >> countLogEventsWith: aClass [

	^ (self allLogEntriesWith: aClass) size
]

{ #category : #running }
EpMonitorIntegrationTest >> setUp [
	super setUp.
	
	logBuilder := EpTestLogBuilder new
		useLogWithSessionStore;
		yourself.
	classFactory := ClassFactoryForTestCase new.
	monitor := EpMonitor newWithLog: logBuilder log.

]

{ #category : #running }
EpMonitorIntegrationTest >> tearDown [

	monitor disable.	
	classFactory cleanUp.
	logBuilder cleanUp.

	super tearDown.
]
