as yet unclassified
notAllConflictsResolved
	"Answer whether any conflicts are unresolved."
	
	^self model anySatisfy: [:item | item isConflict and: [item isResolved not]]