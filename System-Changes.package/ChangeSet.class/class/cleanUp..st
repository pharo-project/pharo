cleanup
cleanUp: aggressive
	"Only delete change sets when being aggressive"

	aggressive
		ifFalse: [ ^ self ].
	ChangeSet removeChangeSetsNamedSuchThat: [ :each | true ].
	ChangeSet resetCurrentToNewUnnamedChangeSet