events
innerPortEvent: aPortEvent 
	aPortEvent isEntityPort ifFalse: [
		"We don't want to resend the transmission, but use a new SimpleTransmission." 
		(self pane port: aPortEvent portName) value: aPortEvent value]. 

	aPortEvent isSelectionPort ifTrue: [
		| index toReplace |
		index := self panes reversed indexOf: aPortEvent pane.
		(self panes reversed first: index - 2) do: [ :each |  
			self removeLastPane.
			self announce: (GLMPaneRemoved pane: each fromBrowser: self) ].
		index <= 1 
			ifTrue: [
				self addPaneFromInner: aPortEvent port.
				self announce: 
					(GLMPaneAdded 
						pane: self panes last 
						atPosition: self panes size 
						inBrowser: self) ]
			ifFalse: [
				toReplace := self removeLastPane.
				self lastActivePresentation: toReplace lastActivePresentation.
				self addPaneFromInner: aPortEvent port.
				self announce: 
					(GLMPaneReplaced 
							oldPane: toReplace 
							newPane: self panes last
							fromBrowser: self ) ] 
	]