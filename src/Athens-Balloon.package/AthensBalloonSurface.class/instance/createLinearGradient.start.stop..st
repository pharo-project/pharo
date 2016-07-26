paints
createLinearGradient: colorRamp start: pt1 stop: pt2
 
	^ AthensBalloonLinearGradient new
		colorRamp: colorRamp;
		origin: pt1;
		corner: pt2;
		yourself