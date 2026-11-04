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

{ #category : 'fill-styles' }
DarkBlueTheme >> progressBarProgressFillColor [

	^ self baseSelectionColor
]

{ #category : 'accessing' }
DarkBlueTheme >> shStyleTableName [

	^ #darkBlueStyleTable
]
