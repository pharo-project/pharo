"
I am the superclass of Metacello's tests: each test runs with a registry of its own, empty, and a plain MetacelloPlatform (the one Iceberg installs clones the repositories it is asked for); both are put back afterwards, even when the test fails. Nothing a test registers or loads reaches the image's registry.
"
Class {
	#name : 'MetacelloTestCase',
	#superclass : 'TestCase',
	#instVars : [
		'previousRegistry',
		'previousPlatform'
	],
	#category : 'Metacello-Tests',
	#package : 'Metacello-Tests'
}

{ #category : 'running' }
MetacelloTestCase >> setUp [

	super setUp.
	previousRegistry := MetacelloProjectRegistry registry.
	previousPlatform := MetacelloPlatform current.
	MetacelloProjectRegistry registry: MetacelloProjectRegistry new.
	MetacelloPlatform current: MetacelloPlatform new
]

{ #category : 'running' }
MetacelloTestCase >> tearDown [

	MetacelloPlatform current: previousPlatform.
	MetacelloProjectRegistry registry: previousRegistry.
	super tearDown
]
