submorphs-add/remove
removeMorph: aMorph
	"Remove the given morph from my submorphs"
	| aWorld |
	aMorph owner == self ifFalse:[^self].
	aWorld := self world.
	aWorld ifNotNil:[
		aMorph outOfWorld: aWorld.
		self privateInvalidateMorph: aMorph.
	].
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph.
