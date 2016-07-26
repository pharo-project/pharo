user-interface
newButtons
	"Answer new buttons as appropriate."

	^{self newFindButton isDefault: true. self newReplaceButton. self newReplaceAllButton. self newCancelButton}