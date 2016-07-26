private registration
unregisterPackage: aPackage forClassName: aClassName
	"Unregister the package back pointer for a given class. The class should not belong to the package anymore before removing the back pointer."

	(aPackage includesClassNamed: aClassName asSymbol) 
		ifTrue: 
			[self error: aPackage name , ' still includes the class ' , aClassName asSymbol].
	^classPackageMapping removeKey: aClassName asSymbol ifAbsent: [self reportBogusBehaviorOf: #unregisterPackage:forClassName:  ]