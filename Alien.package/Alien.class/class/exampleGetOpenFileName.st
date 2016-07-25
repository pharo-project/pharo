examples
exampleGetOpenFileName
	"Call the Win32 GetOpenFileNameA function."
	"Alien exampleGetOpenFileName"
	| openstruct |
	openstruct := OPENFILENAME newC.
	(self lookup: 'GetOpenFileNameA' inLibrary: 'comdlg32.dll')
		primFFICallResult: nil
		with: openstruct pointer.
	^openstruct fileName