*Komitter-Models
initializeWithTarget: targetSnapshot
	| target |	
		
	target := MCDefinitionIndex definitions: targetSnapshot definitions.
	operations := (target definitions collect: [ :definition | MCAddition of: definition ]) asOrderedCollection.