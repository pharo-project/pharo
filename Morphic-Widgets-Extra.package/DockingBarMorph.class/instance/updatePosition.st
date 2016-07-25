private - layout
updatePosition
	"private - update the receiver's position.
	Fixed so as not to keep changing position!
	(called twice if adhereing)"
	
	| edgeSymbol margin |
	edgeSymbol := self edgeToAdhereTo.
	edgeSymbol == #none
		ifTrue: [self
				perform: (edgeSymbol , ':') asSymbol
				with: (self owner perform: edgeSymbol)].
	""
	margin := self avoidVisibleBordersAtEdge
				ifTrue: [self borderWidth asPoint]
				ifFalse: [0 asPoint].
	""
	self isAdheringToTop
		ifTrue: [| usedHeight | 
			usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top ).
			self topLeft: self owner topLeft - margin + (0 @ usedHeight)].
	self isAdheringToBottom
		ifTrue: [| usedHeight | 
			usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#bottom ).
			self bottomLeft: self owner bottomLeft + (-1 @ 1 * margin) - (0 @ usedHeight)].
	""
	self isAdheringToLeft
		ifTrue: [| usedHeight usedWidth | 
			usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top ).
			usedWidth := self usedWidthByPredominantDockingBarsOfChastes: #(#left ).
			self topLeft: self owner topLeft - margin + (usedWidth @ usedHeight)].
	self isAdheringToRight
		ifTrue: [| usedHeight usedWidth | 
			usedHeight := self usedHeightByPredominantDockingBarsOfChastes: #(#top ).
			usedWidth := self usedWidthByPredominantDockingBarsOfChastes: #(#right ).
			self topRight: self owner topRight + (1 @ -1 * margin) + (usedWidth negated @ usedHeight)]