private
simulateOnlyOneClickOn: aMorph
	(Delay forMilliseconds: HandMorph doubleClickTime + 1) wait.
	aMorph simulateClick