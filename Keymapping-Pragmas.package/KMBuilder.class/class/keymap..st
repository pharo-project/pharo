instance creation
keymap: aPlatform
	(#( all Unix MacOSX Windows ) includes: aPlatform) ifFalse: [ self error: aPlatform, ' is not a valid platform' ].
	^self new
		platform: aPlatform;
		yourself