"
A StoredSettingsMergerTest is a test class for testing the behavior of StoredSettingsMerger
"
Class {
	#name : 'StoredSettingsMergerTest',
	#superclass : 'TestCase',
	#instVars : [
		'merger'
	],
	#category : 'System-Settings-Tests',
	#package : 'System-Settings-Tests'
}

{ #category : 'running' }
StoredSettingsMergerTest >> setUp [
	super setUp.
	merger := StoredSettingsMerger new
]

{ #category : 'tests' }
StoredSettingsMergerTest >> testEmpty [
	self assertEmpty: merger storedSettings
]

{ #category : 'tests' }
StoredSettingsMergerTest >> testStoredSettings [
	| numberOfSettings |
	numberOfSettings := MockSettings settings size.
	merger addStoredSettings: MockSettings storedSettings.
	self assert: merger storedSettings size equals: numberOfSettings.
	merger addStoredSettings: MockSettings storedSettings.
	self assert: merger storedSettings size equals: numberOfSettings
]
