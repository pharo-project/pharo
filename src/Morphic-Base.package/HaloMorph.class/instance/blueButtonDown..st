meta-actions
blueButtonDown: event
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	event hand obtainHalo: self.
	positionOffset := event position - (target point: target position in: owner).
	"wait for drags or transfer"
	event hand 
		waitForClicksOrDrag: self 
		event: event
		selectors: { #transferHalo:. nil. nil. #dragTarget:. }
		threshold: 5.