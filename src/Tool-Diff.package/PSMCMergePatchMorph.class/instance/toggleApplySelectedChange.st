as yet unclassified
toggleApplySelectedChange

	self selectedChange remoteChosen 
		ifTrue: [ self selectedChange chooseLocal ]
		ifFalse: [ self selectedChange chooseRemote ].
	self changed: #changes.
	self updateSource