initialization
initializePlatformEncodings
	"Locale initializePlatformEncodings"

	| platform |
	PlatformEncodings ifNil: [ PlatformEncodings := Dictionary new ].

	platform := PlatformEncodings at: 'default' ifAbsentPut: Dictionary new.
	platform
		at: 'default' put: 'iso8859-1';
		at: 'Win32 CE' put: 'utf-8'.

	platform := PlatformEncodings at: 'ja' ifAbsentPut: Dictionary new.
	platform
		at: 'default' put: 'shift-jis';
		at: 'unix' put: 'euc-jp';
		at: 'Win32 CE' put: 'utf-8'.
		
	platform := PlatformEncodings at: 'ko' ifAbsentPut: Dictionary new.
	platform
		at: 'default' put: 'euc-kr';
		at: 'Win32 CE' put: 'utf-8'.

	platform := PlatformEncodings at: 'zh' ifAbsentPut: Dictionary new.
	platform
		at: 'default' put: 'gb2312';
		at: 'unix' put: 'euc-cn';
		at: 'Win32 CE' put: 'utf-8'.
