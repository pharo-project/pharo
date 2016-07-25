macos
valueForMacOSXPlatform: aKeyValue

	^self macOSVirtualKeyTable at: aKeyValue ifAbsent: [ (self basicNew withValue: aKeyValue andName: #Unknown) ] 