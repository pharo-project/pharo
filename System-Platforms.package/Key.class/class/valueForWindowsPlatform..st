windows
valueForWindowsPlatform: aKeyCode

	^self windowsVirtualKeyTable at: aKeyCode ifAbsent: [ (self basicNew withValue: aKeyCode andName: #Unknown) ] 