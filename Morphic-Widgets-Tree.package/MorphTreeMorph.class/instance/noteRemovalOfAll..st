updating
noteRemovalOfAll: aCollection
	"TODO: update the selection as well"

	 self listManager noteRemovalOfAll: aCollection.
	aCollection do: [ :each | each delete ].
	scroller removeAllMorphsIn: aCollection.
	self adjustSubmorphPositions