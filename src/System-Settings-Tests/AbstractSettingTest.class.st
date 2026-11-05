"
I test the settings model built by SettingsBuilder (AbstractSetting and its subclasses).
The settings I use are declared on my class side.
"
Class {
	#name : 'AbstractSettingTest',
	#superclass : 'TestCase',
	#category : 'System-Settings-Tests',
	#package : 'System-Settings-Tests'
}

{ #category : 'settings' }
AbstractSettingTest class >> receiverSettingsOn: aBuilder [

	<receiver_settings_test_data>
	(aBuilder group: #targetedGroup)
		target: Object;
		with: [
			(aBuilder setting: #settingWithoutTarget).
			(aBuilder button: #buttonWithoutTarget) buttonLabel: 'Do' ].
	(aBuilder button: #topLevelButtonWithoutTarget) buttonLabel: 'Do'
]

{ #category : 'settings' }
AbstractSettingTest class >> transientSettingsOn: aBuilder [

	<transient_settings_test_data>
	(aBuilder setting: #persistentSetting)
		readBlock: [ 42 ];
		writeBlock: [ :value | ].
	(aBuilder setting: #transientSetting)
		readBlock: [ 43 ];
		writeBlock: [ :value | ];
		isTransient: true.
	(aBuilder button: #transientButton)
		isTransient: true;
		buttonLabel: 'Do'
]

{ #category : 'helpers' }
AbstractSettingTest >> receiverSettingNamed: aSymbol [

	^ (SettingsBuilder forPragmaNamed: #receiver_settings_test_data) settings detect: [ :each | each name = aSymbol ]
]

{ #category : 'helpers' }
AbstractSettingTest >> settingNamed: aSymbol [

	^ self transientSettings detect: [ :each | each name = aSymbol ]
]

{ #category : 'helpers' }
AbstractSettingTest >> transientSettings [

	^ (SettingsBuilder forPragmaNamed: #transient_settings_test_data) settings
]

{ #category : 'tests - receiver' }
AbstractSettingTest >> testButtonInheritsTheReceiverOfItsGroup [

	self assert: (self receiverSettingNamed: #buttonWithoutTarget) settingReceiver equals: Object
]

{ #category : 'tests - receiver' }
AbstractSettingTest >> testButtonWithoutParentIsReceivedByItsDeclaringClass [

	self assert: (self receiverSettingNamed: #topLevelButtonWithoutTarget) settingReceiver equals: AbstractSettingTest
]

{ #category : 'tests - receiver' }
AbstractSettingTest >> testSettingInheritsTheReceiverOfItsGroup [

	self assert: (self receiverSettingNamed: #settingWithoutTarget) settingReceiver equals: Object
]

{ #category : 'tests - transient' }
AbstractSettingTest >> testBuilderCanDeclareTransientButton [

	| button |
	button := self settingNamed: #transientButton.

	self assert: button class equals: ActionSetting.
	self assert: button isTransient
]

{ #category : 'tests - transient' }
AbstractSettingTest >> testBuilderCanDeclareTransientSetting [

	self assert: (self settingNamed: #transientSetting) isTransient.
	self deny: (self settingNamed: #persistentSetting) isTransient
]

{ #category : 'tests - transient' }
AbstractSettingTest >> testSettingIsNotTransientByDefault [

	self deny: Setting new isTransient.
	self deny: SettingsGroup new isTransient.
	self deny: ActionSetting new isTransient
]

{ #category : 'tests - transient' }
AbstractSettingTest >> testSettingsManagerDoesNotStoreTransientSettings [

	| manager file storedSettings |
	file := FileSystem memory root / 'preferences.ston'.
	manager := SettingsManager forSettings: self transientSettings.
	manager settingsFile: file.

	manager storeSettings.

	storedSettings := manager allStoredSettings.
	self
		assertCollection: (storedSettings collect: [ :each | each settingNodeIdentifier ])
		hasSameElements: { (self settingNamed: #persistentSetting) settingIdentifier }
]

{ #category : 'tests - transient' }
AbstractSettingTest >> testStoredSettingsFactoryIgnoresTransientSettings [

	| storedSettings |
	storedSettings := StoredSettingsFactory new fromSettingNodes: self transientSettings.

	self assert: storedSettings size equals: 1.
	self
		assert: storedSettings first settingNodeIdentifier
		equals: (self settingNamed: #persistentSetting) settingIdentifier.
	self assert: storedSettings first realValue equals: 42
]
