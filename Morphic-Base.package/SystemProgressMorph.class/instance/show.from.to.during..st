private
show: aString from: startNumber to: endNumber during: aBlock

	| progressMorph result |

	self openInWorld.
	progressMorph := self addItemShowing: aString from: startNumber to: endNumber.
		
	self 
		refresh;
		reposition.

	[ result := progressMorph do: aBlock ] ensure: [ self close: progressMorph ].
	^ result.