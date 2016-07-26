unix
valueForUnixPlatform: aKeyValue

	^self unixVirtualKeyTable at: aKeyValue ifAbsent: [ (self basicNew withValue: aKeyValue) ] 