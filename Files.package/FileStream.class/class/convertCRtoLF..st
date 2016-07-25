utils
convertCRtoLF: fileName
	"Convert the given file to LF line endings. Put the result in a file with the extention '.lf'"

	| in out c justPutCR |
	in := (self readOnlyFileNamed: fileName) binary.
	out :=  (self newFileNamed: fileName, '.lf') binary.
	justPutCR := false.
	[in atEnd] whileFalse: [
		c := in next.
		c = 10
			ifTrue: [
				out nextPut: 13.
				justPutCR := true]
			ifFalse: [
				(justPutCR and: [c = 10]) ifFalse: [out nextPut: c].
				justPutCR := false]].
	in close.
	out close.
