Extension { #name : 'WinPlatform' }

{ #category : '*Keymapping-Core' }
WinPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForWindowsPlatform: aKeyValue
]
