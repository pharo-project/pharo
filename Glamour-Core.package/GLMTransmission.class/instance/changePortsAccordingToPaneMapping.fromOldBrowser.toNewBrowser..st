copying
changePortsAccordingToPaneMapping: newPanesMapping fromOldBrowser: aBrowser toNewBrowser: anotherBrowser
	| newPort |
	"newPanesMapping is a dictionary with 
	keys given by old panes and values by new panes"
	self destination: (self destination copyAccordingToPaneMapping: newPanesMapping inNewBrowser: anotherBrowser).
	self originReferences do: [:each | 
		newPort := each port copyAccordingToPaneMapping: newPanesMapping inNewBrowser: anotherBrowser.
		each port: newPort ]