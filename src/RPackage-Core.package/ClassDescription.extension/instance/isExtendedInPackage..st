*rpackage-core
isExtendedInPackage: aPackage
	"returns true if aPackage defines an extension to this class"
	^ aPackage extendsClass: self.