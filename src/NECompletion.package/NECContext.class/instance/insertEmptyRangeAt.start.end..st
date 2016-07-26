private-compute-index
insertEmptyRangeAt: index start: start end: end 
	ranges
		add: (SHRange
				start: start
				end: end
				type: #empty)
		beforeIndex: index