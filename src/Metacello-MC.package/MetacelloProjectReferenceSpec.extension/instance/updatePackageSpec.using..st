*metacello-mc
updatePackageSpec: updatedSpecs using: anMCLoader
	"Add project copy to updatedSpecs if the current version of the project 
	 is different from the receiver's version"
	
	self projectReference updatePackageSpec: updatedSpecs