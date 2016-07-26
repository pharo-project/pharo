actions
relabelTab: aTab with: aStringOrBrick
	"Relabel the tab update tab layout."

	aTab label: aStringOrBrick.
	self updateTabs