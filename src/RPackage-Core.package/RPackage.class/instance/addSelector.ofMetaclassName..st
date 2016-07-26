selector handling
addSelector: aSelector ofMetaclassName: aClassName
	"Add the method to the receiver. If the class is not locally defined in that package then the method is defined as a method extension: ie extending another package. Note that this method does not add the method to the class, it just records in the package that the method is packaged. aClassName is the sole instance class name and not its metaclass one: i.e. adding Point class>>new is done as addSelector: #new ofMetaclassName: #Point"
	
	( self includesClassNamed: aClassName) 
		ifFalse: [(metaclassExtensionSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector]
		ifTrue: [(metaclassDefinedSelectors at: aClassName asSymbol ifAbsentPut: [Set new]) add: aSelector].
	^ aSelector.