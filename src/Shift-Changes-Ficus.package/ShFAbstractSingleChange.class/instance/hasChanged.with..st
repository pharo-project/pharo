checking change
hasChanged: oldDefinition with: newDefinition
	^ (self valueOf: oldDefinition) ~= (self valueOf: newDefinition)