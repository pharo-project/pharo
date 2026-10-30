"
A SettingsStonWriterTest is a test class for testing the behavior of SettingsStonWriter
"
Class {
	#name : 'SettingsStonWriterTest',
	#superclass : 'TestCase',
	#instVars : [
		'writer',
		'fileStream',
		'fileReference'
	],
	#category : 'System-Settings-Tests',
	#package : 'System-Settings-Tests'
}

{ #category : 'running' }
SettingsStonWriterTest >> setUp [
	super setUp.
	fileReference := (FileSystem memory root / 'preferences.ston.txt') asFileReference.
	writer := SettingsStonWriter new.
	fileStream := fileReference writeStream.
	writer stream: fileStream
]

{ #category : 'running' }
SettingsStonWriterTest >> tearDown [
	fileStream close.
	super tearDown
]

{ #category : 'tests' }
SettingsStonWriterTest >> testBasic [
	"Checking that no error occur"

	SettingsStonWriter new store
]

{ #category : 'tests' }
SettingsStonWriterTest >> testEmptyStream [
	fileStream flush.
	self assertEmpty: fileReference contents
]

{ #category : 'tests' }
SettingsStonWriterTest >> testStore [

	| setting |
	setting := MockSettings settingNamed: #booleanSetting.
	writer addSetting: (StoredSettingsFactory new from: setting).
	writer store.
	fileStream flush.
	self assert: fileReference contents equals: (OSPlatform current lineEnding join: MockSettings oneStoredSettingSTONString lines)
]

{ #category : 'tests' }
SettingsStonWriterTest >> testStoreShouldNotCloseBackingStream [

	| setting |
	setting := MockSettings settingNamed: #booleanSetting.
	writer addSetting: (StoredSettingsFactory new from: setting).
	writer store.
	self deny: fileStream closed.
	fileStream flush.
	self assert: fileReference contents equals: (OSPlatform current lineEnding join: MockSettings oneStoredSettingSTONString lines)
]

{ #category : 'tests' }
SettingsStonWriterTest >> testStoreTwoObjects [

	| setting1 setting2 |
	setting1 := MockSettings settingNamed: #booleanSetting.
	setting2 := MockSettings settingNamed: #rangeSetting.
	writer addSetting: ((StoredSettingsFactory new from: setting1)
			 realValue: false;
			 yourself).
	writer addSetting: ((StoredSettingsFactory new from: setting2)
			 realValue: 55;
			 yourself).
	writer store.
	fileStream flush.
	self assert: fileReference contents equals: (OSPlatform current lineEnding join: MockSettings twoStoredSettingsSTONString lines)
]
