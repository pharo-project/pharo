tabs
relabelTab: aTab with: aStringOrMorph
	"Relabel the tab update tab layout."

	aTab label: aStringOrMorph.
	aTab owner ifNotNil: [
		self updateTabs.
		(aTab isSelected and: [aTab isVisible not])
			ifTrue: [self ensureSelectedTabIsVisible]]