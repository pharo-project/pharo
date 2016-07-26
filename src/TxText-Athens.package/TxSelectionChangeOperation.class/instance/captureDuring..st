recording the changes
captureDuring: aBlock

	| before after |
	before := editor selection copy.
	
	aBlock ensure: [ 
		after := editor selection.	
		startDelta := before start -- after start.
		endDelta := before end -- after end.
	].