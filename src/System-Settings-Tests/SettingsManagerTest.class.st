"
I test SettingsManager: building the system settings, storing and loading settings.
The settings I use are declared on my class side and read and write my class-side testValue.
"
Class {
	#name : 'SettingsManagerTest',
	#superclass : 'TestCase',
	#classInstVars : [
		'testValue'
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
		defaultValue: 1
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
SettingsManagerTest >> tearDown [

	self class testValue: nil.
	super tearDown
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

	| manager |
	manager := SettingsManager forPragmaNamed: #settings_manager_test_data.
	manager settingsFile: FileSystem memory root / 'preferences.ston'.
	self class testValue: 2.
	manager storeSettings.

	self class testValue: 3.
	manager loadSettings.

	self assert: self class testValue equals: 2
]
