accessing
prefixMapping
	"use a class inst var so subclasses can define custom mappings"
	^ prefixMapping ifNil: [
		self buildPrefixMapping.
		prefixMapping]