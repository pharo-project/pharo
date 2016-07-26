dispatching
dispatch: anEventBuffer
	| association match |
	self keymapObservers do: [ :aTarget | 
		"nice hack to stop in the first listener"
		aTarget 
			verifyMatchWith: anEventBuffer 
			notifying: self
			thenDoing: [ ^self ] ].
	self noMatch