"
Settings for the FreeType system
"
Class {
	#name : #FreeTypeSystemSettings,
	#superclass : #Model,
	#classVars : [
		'LoadFT2Library'
	],
	#category : #'FreeType-Settings'
}

{ #category : #settings }
FreeTypeSystemSettings class >> freeTypeSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #noFt2Library)
		target: self;
		iconName: #smallConfigurationIcon;
		type: #Label;
		label: 'Free type';
		default: 'Free type fonts are not available';
		precondition: [ FT2Library current isAvailable not ];
		parent: #appearance;
		order: 3;
		description:
			'Free type fonts are not available probably because the FT2 plugin is not installed. Check your VM installation'.
	(aBuilder setting: #loadFt2Library)
		target: self;
		default: true;
		dialog: [ self ft2LibraryVersion ];
		iconName: #smallConfigurationIcon;
		label: 'Use Free type';
		description:
			'If checked, it allows the usage of Freetype fonts and updates available fonts by scanning the current system';
		precondition: [ FT2Library current notNil ];
		parent: #appearance;
		order: 3;
		with: [ (aBuilder pickOne: #monitorType)
				label: 'Monitor type';
				description: 'LCD is generally better for flat screens';
				target: FreeTypeSettings;
				targetSelector: #current;
				order: 0;
				default: #LCD;
				domainValues: {#LCD . #CRT}.
			(aBuilder setting: #updateFontsAtImageStartup)
				order: 1;
				target: FreeTypeSettings;
				default: false;
				label: 'Update fonts at startup';
				description: 'If true, then the available font list is recomputed at each startup'.
			(aBuilder group: #advancedSettings)
				order: 10;
				label: 'Advanced features';
				description: 'Some very specific parameters as the hinting or the cache size';
				with: [ (aBuilder range: #cacheSize)
						target: FreeTypeCache;
						targetSelector: #current;
						label: 'Cache size';
						description: 'The size of the font cache in KBytes';
						default: 5000;
						range: (0 to: 50000 by: 10).
					(aBuilder pickOne: #hintingSymbol)
						label: 'Hinting';
						description:
							'Changes the glyph shapes:'
								,
									'
o FULL: glyph shapes features are snapped to pixel boundaries. Glyphs are monochrome, with no anti-aliasing. This option changes the shapes the most.'
								,
									'
o LIGHT: glyph shapes features are partially snapped to pixel boundaries. This option changes the shapes less than with Full, resulting in better shapes, but less contrast.'
								,
									'
o NORMAL: glyph shapes features are snapped to pixel boundaries. Glyphs are anti-aliased.'
								,
									'
o NONE: use the original glyph shapes without snapping their features to pixel boundaries. This gives the best shapes, but with less contrast and more fuzziness.';
						target: FreeTypeSettings;
						targetSelector: #current;
						default: #Light;
						domainValues:
							{('Light' -> #Light).
							('Normal' -> #Normal).
							('Full' -> #Full).
							('None' -> #None)}.
					(aBuilder range: #glyphContrast)
						target: FreeTypeSettings;
						targetSelector: #current;
						default: 55.0;
						label: 'Glyph contrast';
						description:
							'Change the contrast level for glyphs. This is an integer between 1 and 100' ;
						range: (0 to: 100 by: 10) ] ]
]

{ #category : #settings }
FreeTypeSystemSettings class >> ft2LibraryVersion [
	^ Smalltalk ui theme 
		newLabelIn: self currentWorld 
		for: self 
		label: 'Available version: ', FT2Version current asString 
		getEnabled: nil.
]

{ #category : #settings }
FreeTypeSystemSettings class >> loadFt2Library [
	^ LoadFT2Library ifNil: [LoadFT2Library := false]

]

{ #category : #settings }
FreeTypeSystemSettings class >> loadFt2Library: aBoolean [
	(LoadFT2Library = aBoolean) 
		ifTrue: [ ^ self ].
	LoadFT2Library := aBoolean.
	aBoolean 
		ifTrue: [ 
			FreeTypeFontProvider current updateFromSystem ]
		ifFalse: [
			StandardFonts setSmallBitmapFonts.
			FreeTypeFontProvider unload ]

]

{ #category : #settings }
FreeTypeSystemSettings class >> noFt2Library [
	^ 'Free type fonts are not available'
]

{ #category : #settings }
FreeTypeSystemSettings class >> noFt2Library: aBoolean [ 

	"ignore it for now"

]
