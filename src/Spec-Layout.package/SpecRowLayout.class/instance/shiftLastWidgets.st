private
shiftLastWidgets
	| index ante |
	
	(lastFractionIndex > 0 and: [ lastFractionIndex < size ]) ifFalse: [ ^ self ].
	
	index := size - 1.
	[ index > lastFractionIndex ] whileTrue: [ | myWidth |
		ante := self commands at: index.
		index := index - 1.
		myWidth := ante rightOffset - ante leftOffset.
		ante
			leftOffset: (width + myWidth) negated;
			rightOffset: width negated.
		width := width + myWidth ].
	
	self assert: index = lastFractionIndex.
	
	ante := self commands at: index.
	ante layoutFrame rightOffset: width negated