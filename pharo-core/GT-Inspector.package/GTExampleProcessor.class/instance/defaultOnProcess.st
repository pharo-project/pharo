accessing-defaults
defaultOnProcess
	^ [ :aProcessor :anExample | 
		aProcessor basicProcess: anExample ]