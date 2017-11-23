"
Same as the superclass but also saves the banned rule with a setting
"
Class {
	#name : #RePersistentImageRuleBaner,
	#superclass : #ReImageRuleBaner,
	#category : #'Renraku-Utility'
}

{ #category : #actions }
RePersistentImageRuleBaner class >> ban: aCritique [

	super ban: aCritique.
	
	SystemSettingsPersistence storeIdentifier: 
		ReRuleManager ruleToggleGroupID printString,
		aCritique rule class enabledSettingID printString
		"printString to ensure symbol hash"
]

{ #category : #properties }
RePersistentImageRuleBaner class >> reBanName [

	^ super reBanName, ' and future images'
]
