accessing
classesAndSelectorsDo: aBlock 
	self classesDo: [ :class | 
		self selectorsForClass: class do: [ :sel | 
			aBlock value: class value: sel ]]