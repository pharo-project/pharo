compiling
instVarNamesAndOffsetsDo: aBinaryBlock
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate aBinaryBlock with the instance variable name strings and
	 their integer offsets.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	"Nothing to do here; ClassDescription introduces named instance variables"
	^self