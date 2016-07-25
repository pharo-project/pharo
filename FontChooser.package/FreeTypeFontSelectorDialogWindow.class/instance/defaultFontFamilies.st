as yet unclassified
defaultFontFamilies
	"Answer the set of available fonts families that are supported in the font that they represent."
	
	|fonts defaultFont|
	defaultFont := TextStyle default fontOfPointSize: self theme listFont pointSize.
	fonts := (LogicalFontManager current allFamilies asSortedCollection: [:a :b | 
		a familyName <= b familyName]) collect: [:ff |
			 (ff
				closestMemberWithStretchValue: LogicalFont stretchRegular
				weightValue: LogicalFont weightRegular
				slantValue: LogicalFont slantRegular)
				asLogicalFontOfPointSize: self theme listFont pointSize].
	^fonts collect: [:f | |dispFont|
		dispFont := defaultFont.
		f familyName asText
			addAttribute: (TextFontReference toFont: dispFont)]