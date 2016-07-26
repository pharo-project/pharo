morphic ui
versionSelection: aNumber
	selectedVersion := aNumber isZero
		ifTrue: [ nil ]
		ifFalse: [ versions at: aNumber ].
	self
		changed: #versionSelection;
		changed: #summary