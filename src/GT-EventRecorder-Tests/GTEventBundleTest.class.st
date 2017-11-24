"
A GTEventBundleTest is a test class for testing the behavior of GTEventBundle
"
Class {
	#name : #GTEventBundleTest,
	#superclass : #TestCase,
	#instVars : [
		'bundle',
		'data',
		'category',
		'url',
		'entity'
	],
	#category : #'GT-EventRecorder-Tests-Support'
}

{ #category : #running }
GTEventBundleTest >> setUp [
	super setUp.
	bundle := GTEventBundle new.
	data := #[1 2 3 4].
	category := #category1.
	url := 'http://localhost/gt/events' asZnUrl.
	
]

{ #category : #tests }
GTEventBundleTest >> testData [
	"It is really important that it is the same object."
	bundle data: data.
	self assert: bundle data == bundle data.
]

{ #category : #tests }
GTEventBundleTest >> testEntity [
	bundle data: data.
	entity := bundle entity.
	self assert: (entity isKindOf: ZnByteArrayEntity).
	self assert: entity bytes equals: data.
]
