system integration
fullyRemoveClassNamed: className
	"Remove the class, the class backpointer, the extensions and the extension backPointer from the receiver and the class involved with the class named: className. className is a class name and should not be a metaclass one. "	
	| rPackage |	
		
	rPackage := self packageOfClassNamed: className.
	rPackage ifNil: [ ^ self ].
	rPackage removeClassNamed: className.
	(self extendingPackagesOfClassNamed: className)
		do: [:each | each removeAllMethodsFromClassNamed: className. ].

	
	