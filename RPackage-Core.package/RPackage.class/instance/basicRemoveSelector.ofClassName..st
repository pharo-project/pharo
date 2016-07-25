selector handling
basicRemoveSelector: aSelector ofClassName: aClassName
	"Remove the method in the package but does not propagate to the class itself.  Note that this method does not remove the method from the class, it just records in the package that the method is not in the package anymore."
	
	( self includesClassNamed: aClassName)
		ifFalse: [(classExtensionSelectors  at: aClassName ifAbsent: [ ^ Set new]) remove: aSelector.]
		ifTrue: [(classDefinedSelectors  at: aClassName ifAbsent: [ ^ Set new]) remove: aSelector.].
	
	