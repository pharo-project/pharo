submorphs-add/remove
addMorph: aMorph asElementNumber: aNumber
	"Add the given morph so that it becomes the aNumber'th element of my submorph list.  If aMorph is already one of my submorphs, reposition it"

	(submorphs includes: aMorph) ifTrue:
		[aMorph privateDelete].
	(aNumber <= submorphs size)
		ifTrue:
			[self addMorph: aMorph inFrontOf: (submorphs at: aNumber)]
		ifFalse:
			[self addMorphBack: aMorph]
