checking
checkForModificationsIfCancel: cancelBlock ifMerge: mergeBlock
	| modifications |
	
	"first check if there are local packages which are dirty"
	modifications := self localModifications.
	modifications isEmpty ifTrue: [ ^ self].
	
	"ask the user what to do.. merge | overwrite | abort"
	^ self warnAboutLosingChangesTo: modifications ifCancel: cancelBlock ifMerge: mergeBlock