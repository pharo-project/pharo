instance creation
newLabelBrick: aStringOrBrick selected: selectedStringOrBrick
	"Answer a new label brick with the given label text or brick and
	alternate when selected."
	^ GLMTabLabelBrick new
		label: aStringOrBrick selected: selectedStringOrBrick;
		when: #onClicked send: #onTabClicked:with: to: self;
		tabSelector: self;
		vSpaceFill