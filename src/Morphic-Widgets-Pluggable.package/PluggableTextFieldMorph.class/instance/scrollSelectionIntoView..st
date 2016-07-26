editor access
scrollSelectionIntoView: event 
	"Scroll my text into view if necessary and return true, else return false.
	Redone here to deal with horizontal scrolling!"
	
	| selRects delta selRect rectToTest transform cpHere |
	self selectionInterval: textMorph editor selectionInterval.
	selRects := textMorph paragraph selectionRects.
	selRects isEmpty ifTrue: [^ false].
	rectToTest := selRects first merge: selRects last.
	transform := scroller transformFrom: self.
	(event notNil and: [event anyButtonPressed]) ifTrue:  "Check for autoscroll"
		[cpHere := transform localPointToGlobal: event cursorPoint.
		cpHere x <= self left
			ifTrue: [rectToTest := selRects first topLeft extent: 2@2]
			ifFalse: [cpHere x >= self right
					ifTrue: [rectToTest := selRects last bottomRight extent: 2@2]
					ifFalse: [^ false]]].
	selRect := transform localBoundsToGlobal: rectToTest.
	selRect width > bounds width
		ifTrue: [^ false].  "Would not fit, even if we tried to scroll"
	(delta := selRect amountToTranslateWithin: self innerBounds) ~= (0@0) ifTrue:
		["Scroll end of selection into view if necessary"
		self scrollBy: delta truncated.
		^ true].
	^ false