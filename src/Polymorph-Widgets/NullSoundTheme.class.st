"
A silent sound theme
"
Class {
	#name : #NullSoundTheme,
	#superclass : #SoundTheme,
	#category : 'Polymorph-Widgets-Themes'
}

{ #category : #'as yet unclassified' }
NullSoundTheme class >> themeName [
	"Answer the friendly name of the theme."

	^'No Sounds'
]

{ #category : #initialize-release }
NullSoundTheme >> defaultDefaultSound [
	"Answer the default default sound!"

	^NullSound
]
