testing
includesClassNamed: aSymbol
	"Returns true if the receiver includes class named aSymbol in the classes that are defined within it: only class definition are considered - not class extensions"
	
	^ classes includes: aSymbol asSymbol
	