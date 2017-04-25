change reporting
adoptPaneColor: paneColor
	"Change our color too."

	self assureExtension.

	extension fillStyle
		ifNil: [ self color: paneColor ].
	self borderStyle baseColor: paneColor darker.
	
	self submorphsDo: [ :m | m adoptPaneColor: paneColor ]