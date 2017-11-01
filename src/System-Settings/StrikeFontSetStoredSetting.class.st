"
I am responsible for storing information about StrikeFontSet objects.
"
Class {
	#name : #StrikeFontSetStoredSetting,
	#superclass : #AbstractFontStoredSetting,
	#category : 'System-Settings-Persistence-Ston-StoredSettings'
}

{ #category : #accessing }
StrikeFontSetStoredSetting >> realValue [
	^ (self class environment at: self fontClassName)
				familyName: self familyName 
				size: self pointSize
				emphasized: self emphasized
]
