private
addModelIn: container withSpec: aSpec
	
	self model ifNil: [ ^ self ].
	widget := self model buildWithSpec: aSpec.
	
	container
		addMorph: widget
		fullFrame: LayoutFrame identity