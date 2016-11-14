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
		"contents takeKeyboardFocus" "it should be managed better and not stale focus from another active windows/widgets. It should only forse focus on content when user focused on tabs but it is not clean how to detect it"].
	process ifNotNil: [ processes add: process ].