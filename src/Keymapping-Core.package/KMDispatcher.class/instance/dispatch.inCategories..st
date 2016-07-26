dispatching
dispatch: anEventBuffer inCategories: categories

	(self keymapObserversForCategories: categories) do: [ :aTarget | 
		"nice hack to stop in the first listener"
		aTarget 
			verifyMatchWith: anEventBuffer 
			notifying: self
			thenDoing: [ ^self ] ].
	self noMatch