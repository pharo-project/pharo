accessing
refreshIfNeededStartingAt: firstStreamPosition since: initialLocalName 

	positions isEmptyOrNil ifTrue: [ ^self ]. "Initial #refresh not yet done, so let things untouched."

	(self lastLocalNameAsInteger + 1) asString ~= initialLocalName ifTrue: [
		Halt now. "Shouldn't happen" ^self refresh ].

	"Then, it's only partially loaded i.e. outdated."
	positions addAll: (store entryPositionsStartingAt: firstStreamPosition upTo: endPosition).
