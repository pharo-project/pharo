initialization
initializePresenter
	"Initialize widgets behavior"

	packagesList whenSelectedNodesChanged: [:selection |
		removePackageButton enabled: (self isPackage: selection).
		editPackageButton enabled: selection notEmpty ].
		
	addPackageButton action: [ self addPackage ].
	removePackageButton action: [ self removeSelectedPackage ].
	editPackageButton action: [ self editSelectedPackageRequirements ].