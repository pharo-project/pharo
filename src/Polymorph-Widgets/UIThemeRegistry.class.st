"
Registry for UI themes
"
Class {
	#name : 'UIThemeRegistry',
	#superclass : 'Object',
	#category : 'Polymorph-Widgets-Themes',
	#package : 'Polymorph-Widgets',
	#tag : 'Themes'
}

{ #category : 'accessing' }
UIThemeRegistry class >> icons [
	^ ThemeIcons current
]

{ #category : 'accessing' }
UIThemeRegistry class >> theme [
	^ UITheme current
]
