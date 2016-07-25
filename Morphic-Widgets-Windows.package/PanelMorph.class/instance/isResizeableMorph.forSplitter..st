protocol
isResizeableMorph: submorph forSplitter: e
	| done |
	done := e splitsTopAndBottom
		ifTrue: [ submorph layoutFrame isVerticallyResizeable ]
		ifFalse: [ submorph layoutFrame isHorizontallyResizeable ].
	^ done