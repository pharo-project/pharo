popup - layout
popupLayout: aPopupLayout
	"Change my popup layout to a given one rebuilding popup"

	popupLayout := aPopupLayout.

	self popupLayout
		build: self
		trigger: self triggerBrick
		content: self contentBrick