"
Same as the superclass but also saves the banned rule with a setting
"
Class {
	#name : 'RePersistentImageRuleBaner',
	#superclass : 'ReImageRuleBaner',
	#category : 'Renraku-Utility',
	#package : 'Renraku',
	#tag : 'Utility'
}

{ #category : 'actions' }
RePersistentImageRuleBaner class >> ban: aCritique [

	super ban: aCritique.

	self storeEnabledSettingOf: aCritique rule class in: SettingsManager forSystemSettings
]

{ #category : 'properties' }
RePersistentImageRuleBaner class >> reBanName [

	^ super reBanName, ' and future images'
]

{ #category : 'private' }
RePersistentImageRuleBaner class >> storeEnabledSettingOf: aRuleClass in: aSettingsManager [
	"Store the setting enabling aRuleClass, so that its current state is restored in future images"

	aSettingsManager settings
		detect: [ :each | each name = aRuleClass enabledSettingID ]
		ifFound: [ :setting | aSettingsManager storeSetting: setting ]
]
