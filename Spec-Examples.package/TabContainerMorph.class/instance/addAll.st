private
addAll
	
	self
		addMorph: tab
		fullFrame: ((0 @ 0 corner: 1 @ 0) asLayoutFrame bottomOffset: 35).
				
	self
		addMorph: container
		fullFrame: (LayoutFrame identity topOffset: 35)