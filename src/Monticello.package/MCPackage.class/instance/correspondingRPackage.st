accessing
correspondingRPackage

	^ RPackageOrganizer default packageNamed: self name asSymbol ifAbsent: [ nil ]