Extension { #name : 'WinPlatform' }

{ #category : '*UnifiedFFI' }
WinPlatform >> ffiCallingConvention [
	^ #stdcall
]

{ #category : '*UnifiedFFI' }
WinPlatform >> ffiFloat64Alignment [
	"Windows structure double alignment is 8.
	 Check here: https://msdn.microsoft.com/en-us/library/aa290049(v=vs.71).aspx"
	^ 8
]

{ #category : '*UnifiedFFI' }
WinPlatform >> ffiLibraryName: aLibrary [
	^ aLibrary win32LibraryName
]

{ #category : '*UnifiedFFI' }
WinPlatform >> ffiLongAlignment [
	"In Windows, sizeof(long) == 4 always."
	^ 4
]

{ #category : '*UnifiedFFI' }
WinPlatform >> isTranslated [
	"Tells if a program is emulated through WOW64.
	WOW64 is the x86 emulator that allows 32-bit Windows-based applications to run seamlessly on 64-bit Windows. This allows for 32-bit (x86) Windows applications to run seamlessly in 64-bit (x64) Windows, as well as for 32-bit (x86) and 32-bit (ARM) Windows applications to run seamlessly in 64-bit (ARM64) Windows."

	^ self environment includesKey: 'PROCESSOR_ARCHITEW6432'
]

{ #category : '*UnifiedFFI' }
WinPlatform >> processorArchitecture [
	"Will return the processor architecture:
	 AMD64
    IA64 - Intel Itanium
    ARM64
    EM64T - very rarely seen, almost always on Windows XP-64
    X86 - 32 bit
	"
	| architecture |
	architecture := (self environment at: 'PROCESSOR_ARCHITECTURE') trimBoth.

	^ (architecture asLowercase = 'x86' and: [ self isTranslated ])
		ifTrue: [ (self environment at: 'PROCESSOR_ARCHITEW6432') trimBoth ]
		ifFalse: [ architecture ]
]
