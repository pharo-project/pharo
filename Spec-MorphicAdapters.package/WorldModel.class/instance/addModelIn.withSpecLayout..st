private
addModelIn: container withSpecLayout: aSpec

	self model ifNil: [ ^ self ].
	widget := self model buildWithSpecLayout: aSpec.
	
	container
		addMorph: widget
		fullFrame: LayoutFrame identity