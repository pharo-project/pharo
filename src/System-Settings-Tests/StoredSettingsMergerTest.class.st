"
A StoredSettingsMergerTest is a test class for testing the behavior of StoredSettingsMerger
"
Class {
	#name : #StoredSettingsMergerTest,
	#superclass : #TestCase,
	#instVars : [
		'merger'
	],
	#category : #'System-Settings-Tests'
}

{ #category : #running }
StoredSettingsMergerTest >> setUp [
	super setUp.
	merger := StoredSettingsMerger new.
]

{ #category : #tests }
StoredSettingsMergerTest >> testEmpty [
	self assert: merger storedSettings isEmpty.
]

{ #category : #tests }
StoredSettingsMergerTest >> testStoredSettings [
	| numberOfNodeSettings |
	numberOfNodeSettings := MockSettings nodeSettings size.
	merger addStoredSettings: MockSettings storedSettings.
	self assert: merger storedSettings size equals: numberOfNodeSettings.
	merger addStoredSettings: MockSettings storedSettings.
	self assert: merger storedSettings size equals: numberOfNodeSettings.

]
