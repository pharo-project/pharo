visitor
visitClassModification: aModifiedClassChange
	self compiler evaluate: aModifiedClassChange newClass definitionSource.
		
	self flag: #todo. "Workaround to issue 16836 in fogbugz."
	self
		behaviorNamed: aModifiedClassChange behaviorAffectedName
		ifPresent: [ :aClass | 
			aClass traitComposition: aModifiedClassChange newClass traitComposition.
			aClass theMetaClass traitComposition: aModifiedClassChange newClass theMetaClass traitComposition. ].
