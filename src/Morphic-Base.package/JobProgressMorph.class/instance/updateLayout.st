private
updateLayout

	labelMorph contents isEmpty
		ifFalse: [ self addMorphBack: labelMorph].
	self addMorphBack: bar.