platform specific
defaultEncodingName: languageSymbol 
	| encodings platformName osVersion |
	platformName := Smalltalk os platformName.
	osVersion := Smalltalk os version.
	encodings := self platformEncodings at: languageSymbol
				ifAbsent: [self platformEncodings at: #default].
	encodings at: platformName ifPresent: [:encoding | ^encoding].
	encodings at: platformName , ' ' , osVersion
		ifPresent: [:encoding | ^encoding].
	^encodings at: #default