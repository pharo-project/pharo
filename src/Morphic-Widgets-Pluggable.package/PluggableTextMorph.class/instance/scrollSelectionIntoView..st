editor access
scrollSelectionIntoView: event 
	"Scroll my text into view if necessary and return true, else return false"
	| selRects delta selRect rectToTest transform cpHere editor |
	editor := textMorph editor.
	self selectionInterval: editor selectionInterval.
	selRects := textMorph paragraph selectionRects.
	selRects isEmpty ifTrue: [^ false].
	rectToTest := selRects first merge: selRects last.
	transform := scroller transformFrom: self.
	(event notNil and: [event anyButtonPressed]) ifTrue:  "Check for autoscroll"
		[cpHere := transform localPointToGlobal: event cursorPoint.
		cpHere y <= self top
			ifTrue: [rectToTest := selRects first topLeft extent: 2@2]
			ifFalse: [cpHere y >= self bottom
					ifTrue: [rectToTest := selRects last bottomRight extent: 2@2]
					ifFalse: [^ false]]].
	selRect := transform localBoundsToGlobal: rectToTest.
	selRect height > bounds height
		ifTrue: [(editor pointIndex - editor markIndex) < 0
			ifTrue: [self scrollBy: 0@(self innerBounds top - selRect top)]
			ifFalse: [self scrollBy: 0@(self innerBounds bottom - selRect bottom)].
		^ true].
	(delta := selRect amountToTranslateWithin: self innerBounds) y ~= 0 ifTrue:
		["Scroll end of selection into view if necessary"
		self scrollBy: 0@delta y.
		^ true].
	^ false
