events
outerPortEvent: aPortEvent 
	 
	| toReplace |
	super outerPortEvent: aPortEvent.
	aPortEvent port name = #entity ifFalse: [^self]. 
	self panes isEmpty
		ifFalse: [
			toReplace := self panes first.
			panes := nil.
			self addPaneFromOuter: aPortEvent port.
			self announce: (GLMPaneReplaced 
						oldPane: toReplace 
						newPane: self panes first
						fromBrowser: self ) ]
		ifTrue: [
			self addPaneFromOuter: aPortEvent port.
			self announce: 
				(GLMPaneAdded 
					pane: self panes last 
					atPosition: self panes size 
					inBrowser: self)]