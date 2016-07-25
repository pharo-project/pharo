api
addTab: aTab

	"BEWARE: this code is an exception. One should never access directly a value holder contents"
	"I do this here for optimisation. No need to trigger the whole collection changed. Only the added element is interesting "
	tabs value add: aTab.
	aTab owner: self.
	self selectedTab ifNil: [ selectedTab value: aTab ].
	self changed: #addTab: with: { aTab }