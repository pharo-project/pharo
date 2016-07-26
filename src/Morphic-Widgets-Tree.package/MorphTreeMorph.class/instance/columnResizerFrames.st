column handling
columnResizerFrames
	"return all resizers bounds in a collection - used to update column resizers position"
	| xOffset frms |
	xOffset := 0.
	frms := self columns collect: [:col | 
		xOffset := self minResizerOffset max: (xOffset + col currentWidth).
		scroller left + xOffset @ self top corner: scroller left + xOffset + self resizerWidth @ scroller bottom].
	"If the last column is unbounded, then its frame is removed from the collection because 
	no resizer is added for the last column"
	self lastColumnUnbounded  
		ifTrue: [frms 
			ifNotEmpty: [frms removeLast]].
	^ frms collect: [:f | f translateBy: (scroller offset x negated @ 0)]