private registration
registerExtendingPackage: aPackage forClassName: aClassName 

	| cur |
	cur := classExtendingPackagesMapping at: aClassName asSymbol ifAbsent: [nil].
	cur isNil
		ifTrue: [classExtendingPackagesMapping at: aClassName asSymbol put: (Set with: aPackage)]
		ifFalse: [cur add: aPackage]