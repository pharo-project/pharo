private
show: aString from: startNumber to: endNumber

	| progressMorph |
	self flag: 'When we go from Exceptions to announcements, delete this and use show:from:to:during:'.

	self openInWorld.
	progressMorph := self addItemShowing: aString from: startNumber to: endNumber.

	self 
		refresh;
		reposition.
	
	^ progressMorph.