submorphs-accessing
allMorphs
	"Return a collection containing all morphs in this composite morph (including the receiver)."

	| all |
	all := (Array new: submorphs size) writeStream.
	self allMorphsDo: [:m | all nextPut: m].
	^ all contents