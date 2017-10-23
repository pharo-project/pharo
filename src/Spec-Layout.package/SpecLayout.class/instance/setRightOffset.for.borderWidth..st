initialization
setRightOffset: right for: windowBorderWidth borderWidth: widget

	widget rightFraction = 1
		ifTrue: [ 
			right isZero
				ifTrue: [ widget rightOffset: windowBorderWidth negated ]
				ifFalse: [ widget rightOffset: right - self splitterWidth ] ]