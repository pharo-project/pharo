transitions
zoomIn: goingIn orOutTo: otherImage at: topLeft vanishingPoint: vp 
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40.
	Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40."
	| nSteps minTime startTime |
	nSteps := 16.
	minTime := 500.  "milliseconds"
	startTime := Time millisecondClockValue.
	^ self wipeImage: otherImage at: topLeft clippingBox: nil rectForIndex:
		[:i | | bigR j lilR lead | "i runs from 1 to nsteps"
		i > nSteps
			ifTrue: [nil "indicates all done"]
			ifFalse:
			["If we are going too fast, delay for a bit"
			lead := startTime + (i-1*minTime//nSteps) - Time millisecondClockValue.
			lead > 10 ifTrue: [(Delay forMilliseconds: lead) wait].

			"Return an array with the difference rectangles for this step."
			j := goingIn ifTrue: [i] ifFalse: [nSteps+1-i].
			bigR := vp - (vp*(j)//nSteps) corner:
				vp + (otherImage extent-vp*(j)//nSteps).
			lilR := vp - (vp*(j-1)//nSteps) corner:
				vp + (otherImage extent-vp*(j-1)//nSteps).
			bigR areasOutside: lilR]]