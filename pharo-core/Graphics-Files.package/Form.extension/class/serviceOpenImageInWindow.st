*Graphics-Files-FileRegistry
serviceOpenImageInWindow
	"Answer a service for opening a graphic in a window"

	^ SimpleServiceEntry 
		provider: self 
		label: 'Open graphic in a window'
		selector: #openImageInWindow:
		description: 'Open a graphic file in a window'
		buttonLabel: 'Open'