visitor
visitClassModification: aModifiedClassChange

	self compiler evaluate: aModifiedClassChange oldClass definitionSource.

	self flag: #todo. "Workaround to issue 16836 in fogbugz."
	self
		behaviorNamed: aModifiedClassChange behaviorAffectedName
		ifPresent: [ :aClass | 
			aClass traitComposition: aModifiedClassChange oldClass traitComposition.
			aClass theMetaClass traitComposition: aModifiedClassChange oldClass theMetaClass traitComposition. ].
