query
hasContext: aContext 
	"Answer whether aContext is me or one of my senders"

	^ (self findContextSuchThat: [ :context | context == aContext ]) notNil