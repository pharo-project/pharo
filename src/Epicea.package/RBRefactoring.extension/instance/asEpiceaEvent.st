*Epicea-Refactorings
asEpiceaEvent
	^ (EpGenericRefactoring canAccept: self)
		ifTrue: [ EpGenericRefactoring for: self ]
		ifFalse: [ EpUnknownRefactoring for: self ]