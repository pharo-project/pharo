system compatibility
systemCategories

	"(RPackageOrganizer default packageNamed: 'HelpSystem-Core' asSymbol) systemCategories"
	"(PackageInfo named: 'HelpSystem-Core' asSymbol) systemCategories"
	^ self systemOrganization categories select: [:cat | self includesSystemCategory: cat]