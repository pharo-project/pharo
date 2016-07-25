private
privateMoveBy: delta 
	"Update the bar fillStyle if appropriate."
	
	| fill |
	super privateMoveBy: delta.
	fill := self barFillStyle.
	fill isOrientedFill ifTrue: [fill origin: fill origin + delta]