instance creation
newLabelBrick: aStringOrBrick
	"Answer a new label brick with the given label text."

	^ GLMTabLabelBrick new
		label: aStringOrBrick;
		when: #onClicked send: #onTabClicked:with: to: self;
		tabSelector: self;
		vSpaceFill