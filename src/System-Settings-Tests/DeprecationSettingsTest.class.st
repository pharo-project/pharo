"
I check that the Deprecation handling settings read and write the strategies of Deprecation.
"
Class {
	#name : 'DeprecationSettingsTest',
	#superclass : 'TestCase',
	#category : 'System-Settings-Tests',
	#package : 'System-Settings-Tests'
}

{ #category : 'helpers' }
DeprecationSettingsTest >> assertSetting: aName changes: aSelector to: aStrategy [

	| setting old |
	setting := SettingsManager forSystemSettings settingNamed: aName.
	old := Deprecation perform: aSelector.
	[
		self assert: setting realValue identicalTo: old.
		setting realValue: aStrategy.
		self assert: (Deprecation perform: aSelector) identicalTo: aStrategy ] ensure: [
		Deprecation perform: aSelector asMutator with: old ]
]

{ #category : 'tests' }
DeprecationSettingsTest >> testReactionSettingChangesDeprecation [

	self assertSetting: #reaction changes: #reaction to: IgnoreDeprecationReaction
]

{ #category : 'tests' }
DeprecationSettingsTest >> testRewritingSettingChangesDeprecation [

	self assertSetting: #rewriting changes: #rewriting to: NeverRewriteDeprecation
]
