*Graphics-Files-FileRegistry
serviceImageAsBackground
	"Answer a service for setting the desktop background from a given graphical file's contents"

	^ SimpleServiceEntry 
		provider: self 
		label: 'Use graphic as background'
		selector: #setBackgroundFromImageFileNamed:
		description: 'Use the graphic as the background for the desktop'
		buttonLabel: 'Background'