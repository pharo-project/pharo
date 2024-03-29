"
I am responsible for storing information about StrikeFont objects.
"
Class {
	#name : 'StrikeFontStoredSetting',
	#superclass : 'AbstractFontStoredSetting',
	#category : 'System-Settings-Core-Persistence-Ston-StoredSettings',
	#package : 'System-Settings-Core',
	#tag : 'Persistence-Ston-StoredSettings'
}

{ #category : 'accessing' }
StrikeFontStoredSetting >> realValue [
	^ (self class environment at: self fontClassName)
			familyName: self familyName
			pointSize: self pointSize
			emphasized: self emphasized
]
