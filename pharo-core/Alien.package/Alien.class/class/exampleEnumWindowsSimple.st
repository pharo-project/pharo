examples
exampleEnumWindowsSimple
	"Call the Win32 EnumWindows function, which takes a callback.
	Count how many windows.  Use an Alien to hold the counter on the C heap.
	Answer the count of windows (number of callbacks)."
	"Alien exampleEnumWindowsSimple"
	| count cb |
	count := 0.
	cb := Callback
			block: [:args :result|
					result returnInteger: (count := count + 1)]
			stdcallArgsClass: EnumWindowsProc.
	^((self lookup: 'EnumWindows' inLibrary: 'user32.dll')
		primFFICallResult: (Alien new: 4)
		with: cb thunk
		with: 0) signedLongAt: 1
