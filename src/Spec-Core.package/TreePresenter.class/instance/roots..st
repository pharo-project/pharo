api
roots: aCollection
	"< api: #st getter: #roots registration: #whenRootsChanged:>"
	"Set the tree roots"

	rootsHolder value: aCollection.
	self changed: #setRoots: with: { aCollection }