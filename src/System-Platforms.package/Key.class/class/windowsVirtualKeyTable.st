windows
windowsVirtualKeyTable

	WindowsVirtualKeyTable ifNil: [ self initializeWindowsVirtualKeyTable. ].
	^WindowsVirtualKeyTable