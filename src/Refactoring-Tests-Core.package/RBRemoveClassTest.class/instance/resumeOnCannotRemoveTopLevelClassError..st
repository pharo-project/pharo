private
resumeOnCannotRemoveTopLevelClassError: error
	^ error messageText = RBRemoveClassRefactoring cannotRemoveTopLevelClassErrorMesssage
		ifTrue: [ error resume ]