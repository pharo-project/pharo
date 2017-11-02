"
A SettingsStonWriterTest is a test class for testing the behavior of SettingsStonWriter
"
Class {
	#name : #SettingsStonWriterTest,
	#superclass : #TestCase,
	#instVars : [
		'writer'
	],
	#category : 'System-Settings-Tests'
}

{ #category : #running }
SettingsStonWriterTest >> setUp [
	super setUp.
	writer := SettingsStonWriter new.
	writer stream: (WriteStream on: String new).

]

{ #category : #tests }
SettingsStonWriterTest >> testBasic [
	"Checking that no error occur"
	SettingsStonWriter new store
]

{ #category : #tests }
SettingsStonWriterTest >> testEmptyStream [
	self assert: writer stream contents isEmpty
]

{ #category : #tests }
SettingsStonWriterTest >> testStore [
	| settingNode |
	settingNode := MockSettings booleanSettingNode.
	writer addSetting: (StoredSettingsFactory new from: settingNode).
	writer store.
	
	self assert: writer stream contents equals: MockSettings oneStoredSettingSTONString
]

{ #category : #tests }
SettingsStonWriterTest >> testStoreTwoObjects [
	| settingNode1 settingNode2 |
	settingNode1 := MockSettings booleanSettingNode.
	settingNode2 := MockSettings rangeSettingNode.
	writer addSetting: ((StoredSettingsFactory new from: settingNode1) realValue: false; yourself).
	writer addSetting: ((StoredSettingsFactory new from: settingNode2) realValue: 55; yourself).
	writer store.
	
	self assert: writer stream contents equals: MockSettings twoStoredSettingsSTONString.
]
