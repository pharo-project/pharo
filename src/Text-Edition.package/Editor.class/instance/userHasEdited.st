accessing
userHasEdited
	"Note that my text is free of user edits."
	morph notNil ifTrue:[ morph hasUnacceptedEdits: true ]