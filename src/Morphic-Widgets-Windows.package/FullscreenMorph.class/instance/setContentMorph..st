accessing
setContentMorph: aMorph
	"Replace the submorphs with aMorph."
	
	self removeAllMorphs.
	self
		addMorph: aMorph
		fullFrame: LayoutFrame identity