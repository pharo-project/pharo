protocol
addTab: aTab
	
	(tabs includes: aTab) ifTrue: [ ^ self ].

	tabs add: aTab.

	self registerActionsFor: aTab.
	self selectedTab ifNil: [ aTab selected: true ].
	self addFirstInHistory: aTab.
	self adjustLayout.
	self changed.
	self triggerEvent: #tabAdded with: aTab