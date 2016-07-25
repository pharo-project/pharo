selector handling
addSelector: aSelector ofClassName: aClassName
	"Add the method to the receiver. If the class is not locally defined in that package then the method is defined as a method extension: ie extending another package. Note that this method does not add the method to the class, it just records in the package that the method is packaged."
	
	( self includesClassNamed: aClassName)
		ifFalse: [(classExtensionSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector]
		ifTrue: [(classDefinedSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector].
	^ aSelector.