rendering
actOnPaneReplaced: ann

	container removeMorph: container submorphs last.
	self addMorphFromObject: ann newPane toContainer: container.
	"scrollPane hScrollbar glmAnimateValue: 1.0 duration: 200"