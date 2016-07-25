accessing
allActions 
	^ (self actions, self dynamicActions) 
			select: [ :each | each isAvailableOn: self ]