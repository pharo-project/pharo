open/close
openAsIsIn: aWorld
	"Sad fixup for dodgy layout."

	super openAsIsIn: aWorld.
	self allMorphs do: [:m | m layoutChanged]