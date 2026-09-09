"
Registry for UI themes
"
Class {
	#name : 'UIThemeRegistry',
	#superclass : 'Object',
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
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
