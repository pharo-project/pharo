protected
pluggableSubstitutionMappers

	^pluggableSubstitutions collect: [:aLink |
		FLPluggableSubstitutionMapper 
			when: aLink key
			substituteBy: aLink value]