"
A GTEventRegistryTest is a test class for testing the behavior of GTEventRegistry
"
Class {
	#name : #GTEventRegistryTest,
	#superclass : #TestCase,
	#instVars : [
		'registry'
	],
	#category : #'GT-EventRecorder-Tests-Support'
}

{ #category : #running }
GTEventRegistryTest >> setUp [
	super setUp.
	registry := GTEventRegistry new.
]

{ #category : #'as yet unclassified' }
GTEventRegistryTest >> testIsLiberated [
	"By default it should answer yes."
	self assert: registry isLiberated
]

{ #category : #'as yet unclassified' }
GTEventRegistryTest >> testIsLiberated2 [
	"By default it should answer yes."
	registry object: self.
	self deny: registry isLiberated.
]

{ #category : #'as yet unclassified' }
GTEventRegistryTest >> testIsLiberated3 [
	"By default it should answer yes."
	registry object: self.
	self deny: registry isLiberated.
	registry object: nil.
	self assert: registry isLiberated.
]
