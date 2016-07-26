desktop
desktopGradientOrigin
	"#topLeft #topCenter #topRight #rightCenter #bottomRight #bottomCenter #bottomLeft #leftCenter #center"
	^ DesktopGradientOrigin ifNil: [DesktopGradientOrigin := #topLeft]