"
I am deprecated: use SettingsManager to load and store settings.

I only forward my former class-side API that does not depend on the old setting nodes to SettingsManager.
"
Class {
	#name : 'SystemSettingsPersistence',
	#superclass : 'Object',
	#category : 'System-Settings-Core-Persistence-Ston-Core',
	#package : 'System-Settings-Core',
	#tag : 'Persistence-Ston-Core'
}

{ #category : 'accessing' }
SystemSettingsPersistence class >> defaultPreferenceFileReference [

	self
		deprecated: 'Use SettingsManager class>>#defaultPreferencesFileReference instead. #Pharo15'
		transformWith: '`@receiver defaultPreferenceFileReference' -> 'SettingsManager defaultPreferencesFileReference'.

	^ SettingsManager defaultPreferencesFileReference
]

{ #category : 'system startup' }
SystemSettingsPersistence class >> resumeSystemSettings [

	self
		deprecated: 'Use SettingsManager class>>#resumeSystemSettings instead. #Pharo15'
		transformWith: '`@receiver resumeSystemSettings' -> 'SettingsManager resumeSystemSettings'.

	SettingsManager resumeSystemSettings
]

{ #category : 'storing' }
SystemSettingsPersistence class >> storeIdentifier: aSettingIdentifier [

	self
		deprecated: 'Use SettingsManager>>#storeIdentifier: instead. #Pharo15'
		transformWith: '`@receiver storeIdentifier: `@aSettingIdentifier' -> 'SettingsManager forSystemSettings storeIdentifier: `@aSettingIdentifier'.

	SettingsManager forSystemSettings storeIdentifier: aSettingIdentifier
]

{ #category : 'storing' }
SystemSettingsPersistence class >> storeSettingNodes [

	self
		deprecated: 'Use SettingsManager>>#storeSettings instead. #Pharo15'
		transformWith: '`@receiver storeSettingNodes' -> 'SettingsManager forSystemSettings storeSettings'.

	SettingsManager forSystemSettings storeSettings
]

{ #category : 'loading' }
SystemSettingsPersistence class >> updateSettingNodes [

	self
		deprecated: 'Use SettingsManager>>#loadSettings instead. #Pharo15'
		transformWith: '`@receiver updateSettingNodes' -> 'SettingsManager forSystemSettings loadSettings'.

	SettingsManager forSystemSettings loadSettings
]
