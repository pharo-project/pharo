will be deprecated
fromRectangle: aRectangle
	"Create a margin with four values based on aRectangle using exactly top, left, bottom, right semantics. Pay attention that most of the time you should not use this method but prefer top:left:bottom:right:. The reason is that using rectangle may force you to create rectangles with negative extent which is totally wrong and bogus."
	
	^ self new fromRectangle: aRectangle; yourself