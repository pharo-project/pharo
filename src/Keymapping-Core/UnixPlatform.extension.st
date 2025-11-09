Extension { #name : 'UnixPlatform' }

{ #category : '*Keymapping-Core' }
UnixPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForUnixPlatform: aKeyValue
]
