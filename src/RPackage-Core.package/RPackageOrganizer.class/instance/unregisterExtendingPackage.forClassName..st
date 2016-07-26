private registration
unregisterExtendingPackage: aPackage forClassName: aClassName
	
	| cur |
	cur := classExtendingPackagesMapping at: aClassName asSymbol ifAbsent: [nil].
	cur isNil
		ifFalse: [cur remove: aPackage ifAbsent: [] "not happy with this one"]