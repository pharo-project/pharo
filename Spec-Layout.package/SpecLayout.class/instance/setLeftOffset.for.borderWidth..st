private
setLeftOffset: left for: widget borderWidth: windowBorderWidth

	widget leftFraction = 0
		ifTrue: [ 
			left isZero
				ifTrue: [ widget leftOffset: windowBorderWidth ]
				ifFalse: [ widget leftOffset: left + ProportionalSplitterMorph splitterWidth ] ]
		ifFalse: [ 
			widget leftFraction = 1
				ifFalse: [ widget leftOffset: left + ProportionalSplitterMorph splitterWidth ] ]