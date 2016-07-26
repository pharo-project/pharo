private - testing
shouldCondenseMethod: aMethod
	"Only write methods with changes in the current file (not .sources)"

	^ (SourceFiles fileIndexFromSourcePointer: aMethod sourcePointer) == 2