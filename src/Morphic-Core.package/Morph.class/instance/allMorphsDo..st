submorphs-accessing
allMorphsDo: aBlock 
	"Evaluate the given block for all morphs in this composite morph (including the receiver)."

	submorphs do: [:m | m allMorphsDo: aBlock].
	aBlock value: self