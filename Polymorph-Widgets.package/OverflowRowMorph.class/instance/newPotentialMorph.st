instance creation
newPotentialMorph
	"Answer a morph for trialing potential layout."

	|potential|
	potential := self theme builder newRow: self baseMorphs.
	potential extent: potential minExtent.
	^potential