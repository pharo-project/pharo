accessing
removeEmphasis: aCodeOrTextEmphasis
	self emphasis: 
		(TextEmphasis fromCode: emphasis) remove: (TextEmphasis fromCode: aCodeOrTextEmphasis)