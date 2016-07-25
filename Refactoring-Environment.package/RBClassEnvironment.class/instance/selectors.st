accessing
selectors

	^ Array streamContents: [ :stream |
		self classesDo: [ :class | 
			self selectorsForClass: class do: [ :selector | stream nextPut: selector ] ] ]