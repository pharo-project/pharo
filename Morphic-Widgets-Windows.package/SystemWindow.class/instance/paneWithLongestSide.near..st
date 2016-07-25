resize/collapse
paneWithLongestSide: sideBlock near: aPoint 
	| thePane theSide theLen |
	theLen := 0.
	paneMorphs do:
		[:pane | | box | box := pane bounds.
		box forPoint: aPoint closestSideDistLen:
			[:side :dist :len |
			(dist <= 5 and: [len > theLen]) ifTrue:
				[thePane := pane.
				theSide := side.
				theLen := len]]].
	sideBlock value: theSide.
	^ thePane