private
privateAddMorph: aMorph atIndex: index
	| oldIndex myWorld itsWorld oldOwner |
	(index between: 1 and: submorphs size + 1)
		ifFalse: [ ^ self error: 'index out of range' ].
	myWorld := self world.
	oldOwner := aMorph owner.
	(oldOwner == self and: [ (oldIndex := submorphs indexOf: aMorph) > 0 ])
		ifTrue: [ 
			"aMorph's position changes within in the submorph chain"
			oldIndex < index
				ifTrue: [ 
					"moving aMorph to back"
					submorphs
						replaceFrom: oldIndex
						to: index - 2
						with: submorphs
						startingAt: oldIndex + 1.
					submorphs at: index - 1 put: aMorph ]
				ifFalse: [ 
					"moving aMorph to front"
					oldIndex - 1 to: index by: -1 do: [ :i | submorphs at: i + 1 put: (submorphs at: i) ].
					submorphs at: index put: aMorph ] ]
		ifFalse: [ 
			"adding a new morph"
			oldOwner
				ifNotNil: [ 
					itsWorld := aMorph world.
					itsWorld ifNotNil: [ self privateInvalidateMorph: aMorph ].
					itsWorld == myWorld
						ifFalse: [ aMorph outOfWorld: itsWorld ].
					oldOwner privateRemove: aMorph.
					oldOwner removedMorph: aMorph ].
			aMorph privateOwner: self.
			submorphs := submorphs copyReplaceFrom: index to: index - 1 with: (Array with: aMorph).
			itsWorld == myWorld
				ifFalse: [ aMorph intoWorld: myWorld ] ].
	myWorld ifNotNil: [ self privateInvalidateMorph: aMorph ].
	self layoutChanged.
	oldOwner == self
		ifFalse: [ 
			self addedMorph: aMorph.
			aMorph noteNewOwner: self ]