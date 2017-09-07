rounding
roundedCorners
	"Return a list of those corners to round"
	self wasActivatedFromDockingBar
		ifTrue: [
			activatorDockingBar isFloating
				ifTrue: [^ #(2 3 )].
			activatorDockingBar isAdheringToTop
				ifTrue: [^ #(2 3 )].
			activatorDockingBar isAdheringToBottom
				ifTrue: [^ #(1 4 )].
			activatorDockingBar isAdheringToLeft
				ifTrue: [^ #(3 4 )].
			activatorDockingBar isAdheringToRight
				ifTrue: [^ #(1 2 )]].
	^ super roundedCorners