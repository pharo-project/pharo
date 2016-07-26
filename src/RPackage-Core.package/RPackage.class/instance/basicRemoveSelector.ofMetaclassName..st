selector handling
basicRemoveSelector: aSelector ofMetaclassName: aClassName
	"Remove the method in the package. Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore. aClassName is the sole instance class name and not its metaclass one: i.e. adding Point class>>new is done as removeSelector: #new ofMetaclassName: #Point"
	
	( self includesClassNamed: aClassName)
		ifFalse: [(metaclassExtensionSelectors  at: aClassName ifAbsent: [ ^ Set new]) remove: aSelector.]
		ifTrue: [(metaclassDefinedSelectors  at: aClassName ifAbsent: [ ^ Set new]) remove: aSelector.].
	
	