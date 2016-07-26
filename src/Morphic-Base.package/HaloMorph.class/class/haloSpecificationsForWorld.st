halo theme
haloSpecificationsForWorld
	| desired |
	"Answer a list of HaloSpecs that describe which halos 
	are to be used on a world halo, what they should 
	look like, and where they should be situated"

	desired := #(addDebugHandle: addMenuHandle:   addHelpHandle:  addRecolorHandle:).
	^ self currentHaloSpecifications 
		select: [:spec | desired includes: spec addHandleSelector]