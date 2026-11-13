"
I test that RePersistentImageRuleBaner stores the state of banned rules in the settings.
"
Class {
	#name : 'RePersistentImageRuleBanerTest',
	#superclass : 'TestCase',
	#instVars : [
		'rule',
		'ruleWasEnabled'
	],
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'running' }
RePersistentImageRuleBanerTest >> setUp [

	super setUp.
	rule := ReRuleManager visibleRuleClasses detect: [ :each | each enabledByDefault ].
	ruleWasEnabled := rule enabled
]

{ #category : 'running' }
RePersistentImageRuleBanerTest >> tearDown [

	rule enabled: ruleWasEnabled.
	super tearDown
]

{ #category : 'tests' }
RePersistentImageRuleBanerTest >> testStoreEnabledSettingOfBannedRule [

	| manager setting storedSetting |
	rule enabled: false.
	manager := SettingsManager forSystemSettings.
	manager settingsFile: FileSystem memory root / 'preferences.ston'.

	RePersistentImageRuleBaner storeEnabledSettingOf: rule in: manager.

	setting := manager settingNamed: rule enabledSettingID.
	storedSetting := manager allStoredSettings detect: [ :each | each isForSettingIdentifier: setting settingIdentifier ].
	self deny: storedSetting realValue
]
