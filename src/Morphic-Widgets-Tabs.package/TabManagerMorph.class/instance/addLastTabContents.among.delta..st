private-contents
addLastTabContents: aTab among: size delta: delta
	| process |
	
	process := aTab retrieveMorph: [ :contents | 
		contents ifNil: [ TabWithNoContents signalTab: aTab ].
		contentsWrapper
			addMorph: contents
			fullFrame:
				(LayoutFrame identity
					leftFraction: (size - 1) * delta;
					leftOffset: 2;
					rightFraction: 1).
		"contents takeKeyboardFocus" "it should be managed better and not stale focus from another active windows/widgets. It should only forse focus on content when user focused on tabs but it is not clean how to detect it"].
	
	process ifNotNil: [ processes add: process ]