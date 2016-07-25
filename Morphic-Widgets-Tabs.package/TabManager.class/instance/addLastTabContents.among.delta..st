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
		contents takeKeyboardFocus ].
	
	process ifNotNil: [ processes add: process ]