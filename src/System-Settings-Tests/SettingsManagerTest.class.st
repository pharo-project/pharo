"
I test SettingsManager: building the system settings, storing and loading settings.
The settings I use are declared on my class side and read and write my class-side testValue and otherTestValue.
"
Class {
	#name : 'SettingsManagerTest',
	#superclass : 'TestCase',
	#classInstVars : [
		'testValue',
		'otherTestValue'
	],
	#instVars : [
		'manager'
	],
	#category : 'System-Settings-Tests',
	#package : 'System-Settings-Tests'
}

{ #category : 'settings' }
SettingsManagerTest class >> managerSettingsOn: aBuilder [

	<settings_manager_test_data>
	(aBuilder setting: #testValue)
		readBlock: [ testValue ];
		writeBlock: [ :value | testValue := value ];
		defaultValue: 1.
	(aBuilder setting: #otherTestValue)
		readBlock: [ otherTestValue ];
		writeBlock: [ :value | otherTestValue := value ];
		defaultValue: 10
]

{ #category : 'accessing' }
SettingsManagerTest class >> otherTestValue: anObject [

	otherTestValue := anObject
]

{ #category : 'accessing' }
SettingsManagerTest class >> testValue [

	^ testValue
]

{ #category : 'accessing' }
SettingsManagerTest class >> testValue: anObject [

	testValue := anObject
]

{ #category : 'running' }
SettingsManagerTest >> setUp [

	super setUp.
	manager := SettingsManager forPragmaNamed: #settings_manager_test_data.
	manager settingsFile: FileSystem memory root / 'preferences.ston'
]

{ #category : 'helpers' }
SettingsManagerTest >> storedIdentifiers [

	^ manager allStoredSettings collect: [ :each | each settingNodeIdentifier ]
]

{ #category : 'running' }
SettingsManagerTest >> tearDown [

	self class
		testValue: nil;
		otherTestValue: nil.
	super tearDown
]

{ #category : 'tests' }
SettingsManagerTest >> testDefaultValueIsNotStored [

	self class testValue: 1.

	manager storeSetting: (manager settingNamed: #testValue).

	self assertEmpty: self storedIdentifiers
]

{ #category : 'tests' }
SettingsManagerTest >> testAllSystemSettingsCanBeBuilt [
	"System settings are loaded at startup: every declaration must build with the settings model"

	| settings |
	settings := SettingsManager forSystemSettings settings.

	self denyEmpty: settings.
	self assertEmpty: (settings select: [ :each | each parentName notNil and: [ each hasParent not ] ])
]

{ #category : 'tests' }
SettingsManagerTest >> testForSystemSettingsUsesDefaultPreferencesFile [

	self
		assert: SettingsManager forSystemSettings settingsFile
		equals: SettingsManager defaultPreferencesFileReference
]

{ #category : 'tests' }
SettingsManagerTest >> testLoadSettingsRestoresStoredValues [

	self class
		testValue: 2;
		otherTestValue: 10.
	manager storeSettings.

	self class testValue: 3.
	manager loadSettings.

	self assert: self class testValue equals: 2
]

{ #category : 'tests' }
SettingsManagerTest >> testNonDefaultValueIsStored [

	self class testValue: 2.

	manager storeSetting: (manager settingNamed: #testValue).

	self
		assertCollection: self storedIdentifiers
		hasSameElements: { (manager settingNamed: #testValue) settingIdentifier }
]

{ #category : 'tests' }
SettingsManagerTest >> testStoreSettingsSeparatelyKeepsBoth [

	self class
		testValue: 2;
		otherTestValue: 20.

	manager storeSetting: (manager settingNamed: #testValue).
	manager storeSetting: (manager settingNamed: #otherTestValue).

	self
		assertCollection: self storedIdentifiers
		hasSameElements: (#( #testValue #otherTestValue ) collect: [ :each | (manager settingNamed: each) settingIdentifier ])
]

{ #category : 'tests' }
SettingsManagerTest >> testStoringTheDefaultValueRemovesTheStoredValue [

	self class testValue: 2.
	manager storeSetting: (manager settingNamed: #testValue).

	self class testValue: 1.
	manager storeSetting: (manager settingNamed: #testValue).

	self assertEmpty: self storedIdentifiers
]

{ #category : 'tests' }
SettingsManagerTest >> testSystemSettingGroupNamesAreUnique [
	"Settings find their parent group by name"

	| names |
	names := (SettingsManager forSystemSettings settings select: [ :each | each isGroup ]) collect: [ :each | each name ].

	self assert: names size equals: names asSet size
]

{ #category : 'tests' }
SettingsManagerTest >> testSystemSettingIdentifiersAreUnique [
	"Stored settings are matched by identifier"

	| identifiers |
	identifiers := SettingsManager forSystemSettings settings collect: [ :each | each settingIdentifier ].

	self assert: identifiers size equals: identifiers asSet size
]
