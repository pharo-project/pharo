private
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld otherSubmorphs |
	myWorld := self world.
	otherSubmorphs := submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs := aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs := otherSubmorphs, aCollection]
			ifFalse:[	submorphs := otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner itsWorld |
		itsOwner := m owner.
		itsOwner ifNotNil: [
			itsWorld := m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [self privateInvalidateMorph: m].
				m outOfWorld: itsWorld].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [self privateInvalidateMorph: m].
		(myWorld == itsWorld) ifFalse: [m intoWorld: myWorld].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self layoutChanged.
