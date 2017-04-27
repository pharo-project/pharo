popup - layout
initializeLayout
	"Create trigger and content bricks and let my popup layout
	to build a popup out of them"
	
	self popupLayout
		build: self
		trigger: self newTriggerBrick
		content: self newContentBrick