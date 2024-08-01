"
DarkBlue theme configurator
"
Class {
	#name : 'DarkBlueThemeConfigurator',
	#superclass : 'DarkThemeColorConfigurator',
	#category : 'DarkBlueTheme',
	#package : 'DarkBlueTheme'
}

{ #category : 'colors' }
DarkBlueThemeConfigurator >> backgroundColor [
	^ Color r: 0.116 g: 0.147 b: 0.210
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> balloonBackgroundColor [

	^ self backgroundColor lighter
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> baseColor [

	^ Color r: 0.2 g: 0.24 b: 0.37
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> basePassiveBackgroundColor [
	^ self baseColor lighter
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> baseSelectionColor [
	^ (Color r: 0.243 g: 0.271 b: 0.394 alpha: 1.0)
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> borderColor [
	^ self darkBaseColor adjustBrightness: -0.04
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> darkBaseColor [

	^ self baseColor lighter lighter
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> desktopColor [
	^ self backgroundColor muchDarker
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> growlBorderColorFor: aGrowlMorph [
	^ self borderColor alpha: 0.5
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> growlFillColorFor: aGrowlMorph [
	^ self lightBackgroundColor alpha: 0.5
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> lightBackgroundColor [
	^ Color r: 16r30 g: 16r3b b: 16r4f range: 16rff
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> lightBaseColor [
	^ self baseColor lighter
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> lightSelectionColor [
	^ self baseSelectionColor lighter
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> secondarySelectionColor [
	^ self selectionColor alpha: 0.4
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> selectionColor [
	^ self baseSelectionColor lighter
]
