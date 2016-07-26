accessing
selectedNodeLabel
	| label |
	selectedNode ifNil: [ ^ '' ].
	label := selectedNode sugsMenuLabel.
	^ label copyFrom: 1 to: (35 min: label size)