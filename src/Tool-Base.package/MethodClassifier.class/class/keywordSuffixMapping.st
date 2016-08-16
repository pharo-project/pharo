accessing
keywordSuffixMapping
	"use a class inst var so subclasses can define custom mappings"
	^ keywordSuffixMapping ifNil: [
		self buildKeywordSuffixMapping.
		keywordSuffixMapping]