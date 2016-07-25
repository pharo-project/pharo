private
tryToSelectPaneFrom: aPort
	self panes withIndexDo: [:each :i | 
		(each port: #entity) value = aPort value ifTrue: [
			self announce: 
				(GLMPaneSelected 
					pane: each 
					atPosition: i
					inBrowser: self).
			^ true ] ].
	^ false