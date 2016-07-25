private
shiftLastWidgets
	| index ante |	
	
	(lastFractionIndex > 0 and: [ lastFractionIndex < size ]) ifFalse: [ ^ self ].
	
	index := size - 1.
	[ index > lastFractionIndex ] whileTrue: [ | myHeight |
			ante := self commands at: index.
			index := index - 1.
			myHeight := ante bottomOffset - ante topOffset.
			ante
				topOffset: (height + myHeight) negated;
				bottomOffset: height negated.
			height := height + myHeight ].
		
	self assert: index = lastFractionIndex.
	
	ante := self commands at: index.
	ante bottomOffset: height negated