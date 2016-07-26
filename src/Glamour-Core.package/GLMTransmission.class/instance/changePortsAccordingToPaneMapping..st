copying
changePortsAccordingToPaneMapping: newPanesMapping
	"newPanesMapping is a dictionary with 
	keys given by old panes and values by new panes"
	self destination: ((newPanesMapping at: self destination pane) port: self destination name).
