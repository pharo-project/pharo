private
setTopOffset: top for: widget borderWidth: windowBorderWidth

	widget topFraction = 0
		ifTrue: [
			top isZero
				ifTrue: [ widget topOffset: windowBorderWidth ]
				ifFalse: [ widget topOffset: top + ProportionalSplitterMorph splitterWidth ] ]
		ifFalse: [ 
			widget topFraction = 1
				ifFalse: [ widget topOffset: top + ProportionalSplitterMorph splitterWidth ] ]