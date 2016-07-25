examples
exampleVarArgsEnumFontFamiliesEx
	"Call the Win32 EnumFontFamiliesEx function."
	"Alien exampleEnumFontFamiliesEx"
	"(Time millisecondsToRun: [1000 timesRepeat: [Alien exampleEnumFontFamiliesEx]]) / 1000.0"
	| fontNames r gle err hDC cb |
	fontNames := OrderedCollection new.
	gle := self lookup: 'GetLastError' inLibrary: 'kernel32.dll'.
	hDC := (self lookup: 'GetDC' inLibrary: 'user32.dll')
				primFFICallResult: (Alien new: 4)
				with: 0. "entire screen"
	err := Alien new: 4.
	cb := Callback
			block: [:args :result|
					fontNames addLast: args logicalFontData elfFullName.
					result returnInteger: 1]
			stdcallArgsClass: EnumFontFamExProc.
	(self lookup: 'EnumFontFamiliesExA' inLibrary: 'gdi32.dll')
		primFFICallResult: (r := Alien new: 4)
		withArguments: {hDC.
						 (LOGFONTA newC lfCharset: LOGFONTA DEFAULTCHARSET; yourself) pointer.
						 cb thunk.
						 0.
						 0}.
	(r signedLongAt: 1) ~= 1 ifTrue:
		[gle primFFICallResult: err].
	(self lookup: 'ReleaseDC' inLibrary: 'user32.dll')
		primFFICallResult: (Alien new: 4)
		with: 0 "entire screen"
		with: hDC.
	^(err signedLongAt: 1) -> fontNames -> fontNames size