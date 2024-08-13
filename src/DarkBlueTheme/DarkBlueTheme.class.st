"
Dark blue theme for Pharo.
"
Class {
	#name : 'DarkBlueTheme',
	#superclass : 'PharoDarkTheme',
	#category : 'DarkBlueTheme',
	#package : 'DarkBlueTheme'
}

{ #category : 'configuration' }
DarkBlueTheme class >> colorConfiguratorClass [

	^ DarkBlueThemeConfigurator
]

{ #category : 'accessing' }
DarkBlueTheme class >> themeName [
	^ 'DarkBlue'
]

{ #category : 'border-styles - buttons' }
DarkBlueTheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."

	(aButton valueOfProperty: #noBorder ifAbsent: [ false ])
		ifTrue: [
			^ SimpleBorderStyle new
				width: 0;
				baseColor: Color transparent ].

	^ SimpleBorderStyle new
		width: 1;
		baseColor: self buttonColor darker
]

{ #category : 'accessing' }
DarkBlueTheme >> shStyleTableName [

	^ #darkBlueStyleTable
]

{ #category : 'fill-styles' }
DarkBlueTheme >> windowInactiveFillStyleFor: aWindow [
	"We do not want the lighting effect when the window goes inactive"

	^ SolidFillStyle color: self baseColor slightlyDarker slightlyDarker
]
