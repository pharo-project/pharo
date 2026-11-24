"
Registry for UI themes
"
Class {
	#name : 'UIThemeRegistry',
	#superclass : 'Object',
	#category : 'Morphic-Widgets-Theme',
	#package : 'Morphic-Widgets',
	#tag : 'Theme'
}

{ #category : 'accessing' }
UIThemeRegistry class >> icons [
	^ ThemeIcons current
]

{ #category : 'accessing' }
UIThemeRegistry class >> theme [
	^ UITheme current
]
