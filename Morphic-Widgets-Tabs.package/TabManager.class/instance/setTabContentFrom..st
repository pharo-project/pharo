private
setTabContentFrom: aTab
	| process |
	
	contentsWrapper removeAllMorphs.
	self cleanProcesses.

	process := aTab retrieveMorph: [ :contents |
		contents ifNil: [ TabWithNoContents signalTab: aTab ].
		contentsWrapper 
			addMorph: contents
			fullFrame: LayoutFrame identity.
		contents takeKeyboardFocus ].
	process ifNotNil: [ processes add: process ].