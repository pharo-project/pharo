as yet unclassified
updateFromEvent: anEvent 
	"Update the splitter and attached morph positions from
	the mouse event.
	Take into account the mouse down offset."
	| pNew previousX newWidth minX newLeft |
	pNew := anEvent cursorPoint - lastMouse second.
	minX := index = 1 
		ifTrue: [container minResizerX] 
		ifFalse: [(container columnResizers at: index - 1) right + container minResizerOffset].
	newLeft := minX max: pNew x.
	index = 1 
		ifTrue: [newLeft := newLeft + 3].
	self left: newLeft.
	previousX := index = 1
		ifTrue: [container scroller left - container scroller offset x + 3] 
		ifFalse: [(container columnResizers at: index - 1) left].
	newWidth := self left - previousX.
	(container columns at: index) currentWidth: newWidth.
	container resizerChanged