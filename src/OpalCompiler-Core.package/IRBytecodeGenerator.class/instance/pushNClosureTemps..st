instructions
pushNClosureTemps: n
	n = 0 ifTrue: [ ^ self ].
	n timesRepeat:[ stack push].
	encoder genPushNClosureTemps: n.