testing
updateContents
	"Update the contents."

	|item|
	self contentMorph removeAllMorphs.
	self listSelectionIndex > 0
		ifTrue: [item := (self list at: self listSelectionIndex) copy
					hResizing: #spaceFill;
					vResizing: #rigid.
				self contentMorph
					addMorph: item]