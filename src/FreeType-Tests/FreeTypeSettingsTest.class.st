"
I check the default FreeType settings, on settings of my own.
"
Class {
	#name : 'FreeTypeSettingsTest',
	#superclass : 'TestCase',
	#category : 'FreeType-Tests',
	#package : 'FreeType-Tests'
}

{ #category : 'tests' }
FreeTypeSettingsTest >> testSubPixelAntiAliasingIsOnByDefault [

	| settings |
	settings := FreeTypeSettings new.

	self assert: settings subPixelAntiAliasing equals: settings useSubPixelAntiAliasing
]
