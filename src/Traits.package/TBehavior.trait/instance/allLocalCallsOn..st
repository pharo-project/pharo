obsolete subclasses
allLocalCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	^(SystemNavigation new allLocalCallsOn: aSymbol ofClass: (self theNonMetaClass)).
