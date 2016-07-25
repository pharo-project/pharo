api
hasEditingConflicts: aBoolean
	"Set if the text zone has editing conflicts"
	"<api:#boolean getter: #hasEditingConflicts>"
	
	^ self changed: #hasEditingConflicts: with: { aBoolean }