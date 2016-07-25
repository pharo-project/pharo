desktop
desktopGradientDirection
	"#Radial #Vertical or #Horizontal"
	^ DesktopGradientDirection ifNil: [DesktopGradientDirection := #Vertical]