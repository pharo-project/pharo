font lookup
bestFontFor: aLogicalFont
	"look up best font from the receivers fontProviders"
	
	^self bestFontFor: aLogicalFont whenFindingAlternativeIgnoreAll: Set new
