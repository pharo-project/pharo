private
addPaneFrom: aPort 

	self panes isEmpty ifTrue: [
		self panes addLast: (GLMPane in: self).
		self newTransmission
			addActiveOrigin: aPort; 
			destination: (self panes last port: #entity); 
			transmit
		]
	