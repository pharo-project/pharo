private
addPaneFrom: aPort 
	self panes addLast: self computePane.
	self newTransmission
		addActiveOrigin: aPort; 
		destination: (self panes last port: #entity); 
		presentations: (self presentations copy); 
		transmit