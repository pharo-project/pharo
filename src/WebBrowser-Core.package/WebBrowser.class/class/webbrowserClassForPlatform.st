private - accessing
webbrowserClassForPlatform

	Smalltalk os isUnix ifTrue: [ ^WBUnixWebBrowser ].
	Smalltalk os isWindows ifTrue: [ ^WBWindowsWebBrowser ].
	(Smalltalk os isMacOS or: [ Smalltalk os isMacOSX ])  
		ifTrue: [ ^WBMacWebBrowser ].
	self error: 'No web browser class for platform provided'
