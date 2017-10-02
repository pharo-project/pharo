checking change
fillFrom: oldDefinition to: newDefinition
	^ self
		oldValue: (self valueOf:oldDefinition);
		newValue: (self valueOf:newDefinition);
		oldClass: oldDefinition;
		newClass: newDefinition;
		yourself