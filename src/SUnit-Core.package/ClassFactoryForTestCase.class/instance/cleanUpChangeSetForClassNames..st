cleaning
cleanUpChangeSetForClassNames: classeNames
	| changeSet |
	changeSet := ChangeSet current.
	classeNames do: [:name|
		changeSet 
			removeClassChanges: name;
			removeClassChanges: name, ' class'].	