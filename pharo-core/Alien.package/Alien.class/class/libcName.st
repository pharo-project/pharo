examples
libcName
	| platform |
	platform := SmalltalkImage current platformName.
	platform = 'Mac OS' ifTrue:
		[^SmalltalkImage current osVersion asInteger >= 1070
			ifTrue: ['libobjc.dylib']
			ifFalse:
				[SmalltalkImage current osVersion asInteger >= 1050
					ifTrue: ['libgcc_s.1.dylib']
					ifFalse: ['libc.dylib']]].
	platform = 'unix' ifTrue:
		[SmalltalkImage current osVersion = 'linux-gnu' ifTrue:
			[^'libc.so.6'].
		 ^'libc.so'].
	platform = 'Win32' ifTrue:
		[^'msvcrt.dll'].
	^nil