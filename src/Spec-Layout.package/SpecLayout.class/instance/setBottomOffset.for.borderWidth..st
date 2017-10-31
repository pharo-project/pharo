initialization
setBottomOffset: bottom for: widget borderWidth: windowBorderWidth

	widget bottomFraction = 1
		ifTrue: [ 
			bottom isZero
				ifTrue: [ widget bottomOffset: windowBorderWidth negated ]
				ifFalse: [ widget bottomOffset: bottom - self splitterWidth ] ]