events
windowIsClosing 
	| event |
	event := GLMBrowserClosing new presentation: self.
	self withAllPresentations do: [ :each | each announce: event ]