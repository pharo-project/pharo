private
addPaneFromOuter: aPort 
	"This method adds a new pane at the end of the finder."
	|newPane |
	newPane := self panes addLast: (GLMPane named: (self panes size + 1) asString in: self).
	self newFirstTransmission
		addActiveOrigin: aPort; 
		destination: (newPane port: #entity); 
		transmit