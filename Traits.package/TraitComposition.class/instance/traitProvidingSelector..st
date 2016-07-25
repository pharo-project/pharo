enquiries
traitProvidingSelector: aSymbol
	"Return the trait which originally provides the method aSymbol or return nil
	if trait composition does not provide this selector or there is a conflict.
	Take aliases into account. Return the trait which the aliased method is defined in."

	| methodDescription locatedMethod |
	methodDescription := self methodDescriptionForSelector: aSymbol.
	(methodDescription isProvided not or: [methodDescription isConflict])	
		ifTrue: [^nil].
	locatedMethod := methodDescription providedLocatedMethod.
	^locatedMethod methodClass traitOrClassOfSelector: locatedMethod selector