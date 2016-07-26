private
privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	submorphs := Array 
		new: submorphs size + aCollection size
		streamContents: [:str |
			1 to: index do: [:p | str nextPut: (submorphs atWrap: p)].
			str nextPutAll: aCollection.
			index + 1 to: submorphs size do: [:p | str nextPut: (submorphs atWrap: p)]].
	aCollection do: [:m |  
		m fullBounds. 
		m privateOwner: self].
	self layoutChanged.
