examples
exampleEnumWindowsCallbackPerformance
	"Measure the speed of callbacks using the Win32 EnumWindows function.
	Count how many windows using a Smalltalk counter.
	Answer seconds -> callbacks -> microseconds per callback."
	"Alien exampleEnumWindowsCallbackPerformance"
	| enumWin count ms cb |
	Smalltalk garbageCollect.
	enumWin := self lookup: 'EnumWindows' inLibrary: 'user32.dll'.
	count := 0.
	cb := Callback
			block: [:args :result|
					result returnInteger: (count := count + 1)]
			stdcallArgsClass: EnumWindowsProc.
	ms := Time millisecondsToRun:
			[1000 timesRepeat:
				[enumWin
					primFFICallResult: (Alien new: 4)
					with: cb thunk
					with: 0]].
	^ms / 1000.0 -> count -> (ms * 1000.0 / count)