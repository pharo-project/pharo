private-accessing
shapes

	| lineStream currentLine startPoint endPoint height highlightStartPosition highlightEndPosition |
	shapes ifNotNil: [ ^ shapes ].
	
	shapes := OrderedCollection new.
	self trimIntervalToLayout.
	interval isEmpty ifTrue: [ ^ shapes ].

	highlightStartPosition := self layout positionAt: interval start ifOutside: [ self shouldNotHappen ].
	highlightEndPosition := self layout positionAt: interval end ifOutside: [ self shouldNotHappen ]. 
	
	height := 0.
	lineStream := self layout lines readStream.

	"Skip to line where highlight starts, caching the accumulated y offset for later"	
	[ (currentLine := lineStream next) = highlightStartPosition line ] whileFalse: [ height := height + currentLine height ].

	"The highlight of the first line of the interval will always start at... the start of the interval, duh"
	startPoint := highlightStartPosition topPoint.
	
	"The highlight of each line except the last ends at the right side of the view"
	[ currentLine == highlightEndPosition line ] whileFalse: [  
		endPoint := (layoutView width + layoutView offset x) @ (height + currentLine height).
		shapes add: (startPoint rectangle: endPoint).
		
		height := height + currentLine height.
		currentLine := lineStream next.
		startPoint := 0@height.
	].

	"We're at the last line, end the highlight at the end of the interval"
	endPoint := extendEnd 
		ifTrue: [ (layoutView width + layoutView offset x) @ (height + currentLine height) ]
		ifFalse: [ highlightEndPosition bottomPoint ].
	
	shapes add: (startPoint rectangle: endPoint).

	^ shapes.