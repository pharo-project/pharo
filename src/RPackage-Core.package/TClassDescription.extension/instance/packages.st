*rpackage-core
packages
	"the extending packages of a class are the packages that extend it."
	
	^  self extendingPackages asSet copy add: self package; yourself