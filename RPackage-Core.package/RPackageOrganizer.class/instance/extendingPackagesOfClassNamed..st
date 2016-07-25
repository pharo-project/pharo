package access from class
extendingPackagesOfClassNamed: aName
	"Returns the packages extending the class named a Symbol"
	^ classExtendingPackagesMapping at: aName asSymbol ifAbsent: [#()]