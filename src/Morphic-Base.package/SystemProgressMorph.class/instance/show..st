private
show: aJob

	| progressMorph |

	self flag: 'When we go from Exceptions to announcements, delete this and use show:from:to:during:'.
	
	progressMorph := self addItemShowing: aJob.
	
	self openInWorld.
	self updateWidth.
	
	^ progressMorph.