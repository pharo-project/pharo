private
addPaneFrom: aPort 
	aPort value isNil ifTrue: [ ^ self ].
	(self tryToSelectPaneFrom: aPort) ifTrue: [ ^ self ].
	self panes addLast: (GLMPane named: (self panes size + 1) asString in: self).
	self newTransmission
		addActiveOrigin: aPort; 
		destination: (self panes last port: #entity); 
		transmit.
	self announce: 
			(GLMPaneAdded 
				pane: self panes last 
				atPosition: self panes size 
				inBrowser: self)