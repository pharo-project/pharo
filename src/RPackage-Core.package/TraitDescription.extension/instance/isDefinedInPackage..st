*rpackage-core
isDefinedInPackage: aPackage
	"returns true if aPackage contains the definitino of this class"
	
	^ aPackage includesClass: self.