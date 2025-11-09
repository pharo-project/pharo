Extension { #name : 'MacOSPlatform' }

{ #category : '*Keymapping-Core' }
MacOSPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForMacOSXPlatform: aKeyValue
]

{ #category : '*Keymapping-Core' }
MacOSPlatform >> shortcutPrinter [

	^ KMOSXShortcutPrinter new
]
