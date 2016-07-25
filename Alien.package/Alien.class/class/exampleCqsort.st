examples
exampleCqsort
	"Call the libc qsort function (which requires a callback)."
	"Alien exampleCqsort"
	"(Time millisecondsToRun: [100 timesRepeat: [Alien exampleCqsort]]) / 100.0"
	| cb rand nElements sizeofDouble values orig sort |
	rand := Random new.
	values := Alien newC: (nElements := 100) * (sizeofDouble := 8).
	1 to: values dataSize by: sizeofDouble do:
		[:i| values doubleAt: i put: rand next].
	orig := (1 to: values dataSize by: sizeofDouble) collect: [:i| values doubleAt: i].
	cb := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].
	(Alien lookup: 'qsort' inLibrary: Alien libcName)
		primFFICallResult: nil
		with: values pointer
		with: nElements
		with: sizeofDouble
		with: cb thunk.
	sort := (1 to: values dataSize by: sizeofDouble) collect: [:i| values doubleAt: i].
	values free.
	^orig -> sort