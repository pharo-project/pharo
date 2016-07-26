development support
updatePackageSpec: updatedSpecs using: anMCLoader
	"Add pkg copy to updatedSpecs if the file in current image is different from the receiver's file"
	
	self updatePackageSpec: updatedSpecs force: false using: anMCLoader