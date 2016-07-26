examples
exampleEnumWindows
	"Call the Win32 EnumWindows function, which takes a callback.
	Count how many windows.  Use an Alien to hold the counter on the C heap.
	Answer the count of windows (number of callbacks)."
	"Alien exampleEnumWindows"
	| counter cb |
	counter := Alien newC: 4.
	cb := Callback
				block: [:args :result| | ptr |
						ptr := Alien forPointer: args lParam.
						ptr signedLongAt: 1 put: (ptr signedLongAt: 1) + 1.
						result returnInteger: (ptr signedLongAt: 1)]
				stdcallArgsClass: EnumWindowsProc.
	^((self lookup: 'EnumWindows' inLibrary: 'user32.dll')
		primFFICallResult: (Alien new: 4)
		with: cb thunk
		with: counter pointer) signedLongAt: 1
