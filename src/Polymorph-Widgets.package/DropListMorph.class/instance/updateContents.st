testing
updateContents
	"Update the contents."

	self updateContentMorphWith:
		(self listSelectionIndex > 0
			ifTrue: [self listMorph wrapItem: (self list at: self listSelectionIndex) index: self listSelectionIndex ]
			ifFalse: [ self defaultContents ]) 