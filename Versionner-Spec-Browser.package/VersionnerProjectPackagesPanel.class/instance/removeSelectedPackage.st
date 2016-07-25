actions
removeSelectedPackage
	"remove the selected package from the model and the list."
	| selectedPackage index |

	selectedPackage := packagesList selectedItem.
	selectedPackage ifNil: [ ^self ].
	
	selectedPackage content remove.
	"index := packagesList selectedIndex."
	packagesList roots: project packages.
	"packagesList setSelectedIndex: index."
	
