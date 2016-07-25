*Regex-Core
copyWithRegex: rxString matchesTranslatedUsing: aBlock

	^rxString asRegex
		copy: self translatingMatchesUsing: aBlock