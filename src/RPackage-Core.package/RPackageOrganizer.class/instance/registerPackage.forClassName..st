private registration
registerPackage: aPackage forClassName: aClassNameSymbol
	"Register aPackage as the package of the class aClassNameSymbol. The package should contain the class in its class definitions."
	
	(aPackage includesClassNamed: aClassNameSymbol asSymbol) 
		ifFalse: 
			[self error: aPackage name , ' does not include the class ' , aClassNameSymbol].
	^classPackageMapping at: aClassNameSymbol put: aPackage