testing
shouldDisplayContext: aContext basedOnFilters: stackFilters
	"Return true if all filters match the given context, or false otherwise.
	True is returned if there are no filters."
		
	^stackFilters allSatisfy: [ :aFilter | aFilter shouldDisplay: aContext ].
	