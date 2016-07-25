testing
ignoreUnsavedChangesOnDevelopmentIfAny
	^ self hasChanges
		ifTrue: [ self confirm: 'There are unsaved changes to the development version.
Ok to loose these changes?' ]
		ifFalse: [ true ]