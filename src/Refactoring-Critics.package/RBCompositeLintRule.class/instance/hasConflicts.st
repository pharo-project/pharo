testing
hasConflicts
	^ rules anySatisfy: [ :each | each hasConflicts ]