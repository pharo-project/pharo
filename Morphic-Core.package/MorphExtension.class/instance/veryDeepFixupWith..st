connectors-copying
veryDeepFixupWith: deepCopier 
	"If target and arguments fields were weakly copied, fix them here.
	If they were in the tree being copied, fix them up, otherwise point to the originals!!"

	super veryDeepFixupWith: deepCopier.
	otherProperties ifNil: [ ^self ].

	"Properties whose values are only copied weakly replace those values if they were copied via another path"
	self copyWeakly do: [ :propertyName |
		otherProperties at: propertyName ifPresent: [ :property |
			otherProperties at: propertyName
				put: (deepCopier references at: property ifAbsent: [ property ])]].
