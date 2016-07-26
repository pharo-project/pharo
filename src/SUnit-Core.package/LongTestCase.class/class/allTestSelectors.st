accessing
allTestSelectors
	self shouldRun ifTrue: [
		^super testSelectors].
	^#().